{
  This file is part of PasBuild.

  Copyright (c) 2025-2026 Graeme Geldenhuys <graemeg@gmail.com>

  SPDX-License-Identifier: BSD-3-Clause

  See LICENSE file in the project root for full license terms.
}

unit PasBuild.Remote;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, ssockets,
  PasBuild.Dependencies,
  PasBuild.Crypto.SHA256;

type
  { StatusCode carries the HTTP status (0 for transport-level failures
    such as DNS errors or connection refusals) so callers can tell
    "package not in this registry" (404) from "registry unreachable" }
  ENetworkError = class(EDependencyError)
  private
    FStatusCode: Integer;
  public
    constructor CreateWithStatus(AStatusCode: Integer; const AMessage: string);
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;

  EChecksumError = class(EDependencyError);

  TRemoteClient = class
  private
    FConnectTimeout: Integer;
    FReadTimeout: Integer;
    function CreateHttpClient: TFPHTTPClient;
    procedure ValidateChecksum(const AFilePath, AExpectedSHA256: string);
  public
    constructor Create(AConnectTimeout: Integer = 30;
      AReadTimeout: Integer = 60);

    { Raise ENetworkError unless the URL uses https://, or http:// to a
      loopback address (local test registries). Manifests and archives
      must not travel over cleartext HTTP where they can be rewritten
      on-path. }
    class procedure ValidateUrlScheme(const AUrl: string);

    function FetchText(const AUrl: string): string;
    procedure DownloadFile(const AUrl, ADestPath: string;
      const AExpectedSHA256: string = '');

    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
  end;

{ Enable TLS certificate verification on an HTTP client when a system CA
  bundle can be located (FPC 3.2.2 performs no peer verification by
  default). Logs a one-time warning and leaves verification off when no
  CA bundle is found, rather than breaking every HTTPS fetch. }
procedure AttachTlsVerification(AClient: TFPHTTPClient);

{ Search ASearchDirs in order for a matching libssl/libcrypto dylib pair,
  preferring OpenSSL 3 over 4 within a directory. On success ALibDir is
  the directory with a trailing slash and AVersion the dylib version
  suffix (e.g. '.3'). FPC 3.2.2's OpenSSL unit only probes for 1.1 and
  older on macOS, and Homebrew/MacPorts locations are not on the dyld
  search path, so PasBuild has to point it at a modern install. }
function FindDarwinOpenSSL(const ASearchDirs: array of string;
  out ALibDir, AVersion: string): Boolean;

implementation

uses
  sslsockets, opensslsockets, openssl,
  PasBuild.Utils;

function FindDarwinOpenSSL(const ASearchDirs: array of string;
  out ALibDir, AVersion: string): Boolean;
const
  Versions: array[0..1] of string = ('.3', '.4');
var
  Dir, Ver: string;
begin
  Result := False;
  ALibDir := '';
  AVersion := '';
  for Dir in ASearchDirs do
    for Ver in Versions do
      if FileExists(IncludeTrailingPathDelimiter(Dir) + 'libssl' + Ver + '.dylib') and
         FileExists(IncludeTrailingPathDelimiter(Dir) + 'libcrypto' + Ver + '.dylib') then
      begin
        ALibDir := IncludeTrailingPathDelimiter(Dir);
        AVersion := Ver;
        Exit(True);
      end;
end;

{$IFDEF DARWIN}
procedure ConfigureDarwinOpenSSL;
const
  SearchDirs: array[0..5] of string = (
    '/opt/homebrew/opt/openssl@3/lib',  // Homebrew, Apple Silicon
    '/usr/local/opt/openssl@3/lib',     // Homebrew, Intel
    '/opt/homebrew/lib',
    '/usr/local/lib',
    '/opt/local/lib',                   // MacPorts
    '/opt/homebrew/opt/openssl@4/lib'
  );
var
  LibDir, Version: string;
begin
  if not FindDarwinOpenSSL(SearchDirs, LibDir, Version) then
    Exit; // fall back to the RTL's own search (OpenSSL 1.1 and older)
  DLLSSLName := LibDir + 'libssl';
  DLLUtilName := LibDir + 'libcrypto';
  // LoadLibraries replaces the first (unversioned) entry with the second
  DLLVersions[2] := Version;
end;
{$ENDIF}

type
  TTlsVerificationHelper = class
    procedure HandleGetSocketHandler(Sender: TObject; const UseSSL: Boolean;
      out AHandler: TSocketHandler);
  end;

var
  GTlsHelper: TTlsVerificationHelper;
  GWarnedNoCABundle: Boolean;

function FindSystemCABundle: string;
const
  Candidates: array[0..3] of string = (
    '/etc/ssl/certs/ca-certificates.crt', // Debian/Ubuntu
    '/etc/pki/tls/certs/ca-bundle.crt',   // RHEL/Fedora
    '/etc/ssl/ca-bundle.pem',             // openSUSE
    '/etc/ssl/cert.pem'                   // FreeBSD/OpenBSD/macOS/Alpine
  );
var
  I: Integer;
begin
  Result := '';
  for I := Low(Candidates) to High(Candidates) do
    if FileExists(Candidates[I]) then
      Exit(Candidates[I]);
end;

procedure TTlsVerificationHelper.HandleGetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
var
  CABundle: string;
begin
  AHandler := nil; // non-SSL: let the client create its default handler
  if not UseSSL then
    Exit;

  AHandler := TSSLSocketHandler.GetDefaultHandler;
  CABundle := FindSystemCABundle;
  if CABundle <> '' then
  begin
    TSSLSocketHandler(AHandler).VerifyPeerCert := True;
    TSSLSocketHandler(AHandler).CertificateData.CertCA.FileName := CABundle;
  end
  else if not GWarnedNoCABundle then
  begin
    GWarnedNoCABundle := True;
    TUtils.LogWarning('No system CA certificate bundle found; TLS ' +
      'certificate verification is disabled for this run.');
  end;
end;

procedure AttachTlsVerification(AClient: TFPHTTPClient);
begin
  if GTlsHelper = nil then
    GTlsHelper := TTlsVerificationHelper.Create;
  AClient.OnGetSocketHandler := @GTlsHelper.HandleGetSocketHandler;
end;

{ ENetworkError }

constructor ENetworkError.CreateWithStatus(AStatusCode: Integer;
  const AMessage: string);
begin
  inherited Create(AMessage);
  FStatusCode := AStatusCode;
end;

{ TRemoteClient }

constructor TRemoteClient.Create(AConnectTimeout: Integer;
  AReadTimeout: Integer);
begin
  inherited Create;
  FConnectTimeout := AConnectTimeout;
  FReadTimeout := AReadTimeout;
end;

class procedure TRemoteClient.ValidateUrlScheme(const AUrl: string);
var
  LowerUrl, Host: string;
  SlashPos, ColonPos: Integer;
begin
  LowerUrl := LowerCase(AUrl);

  if Copy(LowerUrl, 1, 8) = 'https://' then
    Exit;

  if Copy(LowerUrl, 1, 7) = 'http://' then
  begin
    // Extract the host component (up to the next '/', stripping any port)
    Host := Copy(LowerUrl, 8, MaxInt);
    SlashPos := Pos('/', Host);
    if SlashPos > 0 then
      Host := Copy(Host, 1, SlashPos - 1);
    if (Host <> '') and (Host[1] = '[') then
    begin
      // Bracketed IPv6 literal, e.g. [::1]:9000
      ColonPos := Pos(']', Host);
      if ColonPos > 0 then
        Host := Copy(Host, 1, ColonPos);
    end
    else
    begin
      ColonPos := Pos(':', Host);
      if ColonPos > 0 then
        Host := Copy(Host, 1, ColonPos - 1);
    end;

    if (Host = 'localhost') or (Host = '[::1]') or
       (Copy(Host, 1, 4) = '127.') then
      Exit;

    raise ENetworkError.CreateFmt(
      'Refusing cleartext HTTP URL "%s". Registries and package origins ' +
      'must use https:// (http:// is only allowed for loopback addresses).',
      [AUrl]);
  end;

  raise ENetworkError.CreateFmt(
    'Unsupported URL scheme in "%s". Only https:// is allowed ' +
    '(http:// for loopback addresses).', [AUrl]);
end;

function TRemoteClient.CreateHttpClient: TFPHTTPClient;
begin
  Result := TFPHTTPClient.Create(nil);
  Result.ConnectTimeout := FConnectTimeout * 1000;
  Result.IOTimeout := FReadTimeout * 1000;
  Result.AllowRedirect := True;
  Result.AddHeader('User-Agent', 'PasBuild/1.0');
  AttachTlsVerification(Result);
end;

function TRemoteClient.FetchText(const AUrl: string): string;
var
  Client: TFPHTTPClient;
  ResponseCode: Integer;
  Retried: Boolean;
begin
  ValidateUrlScheme(AUrl);

  Retried := False;
  Client := CreateHttpClient;
  try
    try
      Result := Client.Get(AUrl);
      ResponseCode := Client.ResponseStatusCode;
    except
      on E: ESocketError do
        raise ENetworkError.CreateFmt(
          'Cannot connect to %s: %s', [AUrl, E.Message]);
      on E: EHTTPClient do
      begin
        ResponseCode := Client.ResponseStatusCode;
        if (ResponseCode >= 500) and (not Retried) then
        begin
          Retried := True;
          Sleep(2000);
          Client.Free;
          Client := nil; // guard the outer finally if the next line raises
          Client := CreateHttpClient;
          try
            Result := Client.Get(AUrl);
            ResponseCode := Client.ResponseStatusCode;
          except
            on E2: Exception do
              raise ENetworkError.CreateWithStatus(ResponseCode, Format(
                'Registry returned server error (%d) at %s: %s',
                [ResponseCode, AUrl, E2.Message]));
          end;
        end
        else
          raise ENetworkError.CreateWithStatus(ResponseCode, Format(
            'HTTP error %d fetching %s: %s',
            [ResponseCode, AUrl, E.Message]));
      end;
    end;

    if (ResponseCode >= 400) and (ResponseCode < 500) then
      raise ENetworkError.CreateWithStatus(ResponseCode, Format(
        'HTTP %d: Resource not found at %s', [ResponseCode, AUrl]));

    if ResponseCode >= 500 then
      raise ENetworkError.CreateWithStatus(ResponseCode, Format(
        'Registry returned server error (%d) at %s',
        [ResponseCode, AUrl]));
  finally
    Client.Free;
  end;
end;

procedure TRemoteClient.DownloadFile(const AUrl, ADestPath: string;
  const AExpectedSHA256: string);
var
  Client: TFPHTTPClient;
  FS: TFileStream;
  ResponseCode: Integer;
  DestDir, TempPath: string;
begin
  ValidateUrlScheme(AUrl);

  DestDir := ExtractFileDir(ADestPath);
  if (DestDir <> '') and (not DirectoryExists(DestDir)) then
    ForceDirectories(DestDir);

  // Download to a process-unique temp name and rename into place only
  // after the checksum verifies: an interrupted download can never be
  // mistaken for a valid cached archive, and concurrent pasbuild
  // processes cannot truncate each other's in-progress downloads
  TempPath := ADestPath + '.tmp.' + IntToStr(GetProcessID);

  Client := CreateHttpClient;
  try
    try
      FS := TFileStream.Create(TempPath, fmCreate);
      try
        try
          Client.Get(AUrl, FS);
          ResponseCode := Client.ResponseStatusCode;
        except
          on E: ESocketError do
            raise ENetworkError.CreateFmt(
              'Cannot connect to %s: %s', [AUrl, E.Message]);
          on E: EHTTPClient do
            raise ENetworkError.CreateFmt(
              'HTTP error downloading %s: %s', [AUrl, E.Message]);
        end;
      finally
        FS.Free;
      end;

      if ResponseCode >= 400 then
        raise ENetworkError.CreateFmt(
          'HTTP %d downloading %s', [ResponseCode, AUrl]);

      if AExpectedSHA256 <> '' then
        ValidateChecksum(TempPath, AExpectedSHA256);

      if FileExists(ADestPath) then
        DeleteFile(ADestPath);
      if not RenameFile(TempPath, ADestPath) then
        raise ENetworkError.CreateFmt(
          'Cannot move downloaded file into place: %s', [ADestPath]);
    except
      if FileExists(TempPath) then
        DeleteFile(TempPath);
      raise;
    end;
  finally
    Client.Free;
  end;
end;

procedure TRemoteClient.ValidateChecksum(const AFilePath,
  AExpectedSHA256: string);
var
  Actual: string;
begin
  Actual := SHA256File(AFilePath);
  if LowerCase(Actual) <> LowerCase(AExpectedSHA256) then
  begin
    DeleteFile(AFilePath);
    raise EChecksumError.CreateFmt(
      'Checksum verification failed. Expected: %s, got: %s. ' +
      'The archive may have been tampered with.',
      [LowerCase(AExpectedSHA256), LowerCase(Actual)]);
  end;
end;

initialization
{$IFDEF DARWIN}
  ConfigureDarwinOpenSSL;
{$ENDIF}

finalization
  GTlsHelper.Free;

end.
