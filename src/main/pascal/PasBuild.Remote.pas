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
  ENetworkError = class(EDependencyError);
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

    function FetchText(const AUrl: string): string;
    procedure DownloadFile(const AUrl, ADestPath: string;
      const AExpectedSHA256: string = '');

    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
  end;

implementation

uses
  opensslsockets;

{ TRemoteClient }

constructor TRemoteClient.Create(AConnectTimeout: Integer;
  AReadTimeout: Integer);
begin
  inherited Create;
  FConnectTimeout := AConnectTimeout;
  FReadTimeout := AReadTimeout;
end;

function TRemoteClient.CreateHttpClient: TFPHTTPClient;
begin
  Result := TFPHTTPClient.Create(nil);
  Result.ConnectTimeout := FConnectTimeout * 1000;
  Result.IOTimeout := FReadTimeout * 1000;
  Result.AllowRedirect := True;
  Result.AddHeader('User-Agent', 'PasBuild/1.0');
end;

function TRemoteClient.FetchText(const AUrl: string): string;
var
  Client: TFPHTTPClient;
  ResponseCode: Integer;
  Retried: Boolean;
begin
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
          Client := CreateHttpClient;
          try
            Result := Client.Get(AUrl);
            ResponseCode := Client.ResponseStatusCode;
          except
            on E2: Exception do
              raise ENetworkError.CreateFmt(
                'Registry returned server error (%d) at %s: %s',
                [ResponseCode, AUrl, E2.Message]);
          end;
        end
        else
          raise ENetworkError.CreateFmt(
            'HTTP error %d fetching %s: %s',
            [ResponseCode, AUrl, E.Message]);
      end;
    end;

    if (ResponseCode >= 400) and (ResponseCode < 500) then
      raise ENetworkError.CreateFmt(
        'HTTP %d: Resource not found at %s', [ResponseCode, AUrl]);

    if ResponseCode >= 500 then
      raise ENetworkError.CreateFmt(
        'Registry returned server error (%d) at %s',
        [ResponseCode, AUrl]);
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
  DestDir: string;
begin
  DestDir := ExtractFileDir(ADestPath);
  if (DestDir <> '') and (not DirectoryExists(DestDir)) then
    ForceDirectories(DestDir);

  Client := CreateHttpClient;
  try
    FS := TFileStream.Create(ADestPath, fmCreate);
    try
      try
        Client.Get(AUrl, FS);
        ResponseCode := Client.ResponseStatusCode;
      except
        on E: ESocketError do
        begin
          FreeAndNil(FS);
          DeleteFile(ADestPath);
          raise ENetworkError.CreateFmt(
            'Cannot connect to %s: %s', [AUrl, E.Message]);
        end;
        on E: EHTTPClient do
        begin
          FreeAndNil(FS);
          DeleteFile(ADestPath);
          raise ENetworkError.CreateFmt(
            'HTTP error downloading %s: %s', [AUrl, E.Message]);
        end;
      end;
    finally
      FS.Free;
    end;

    if ResponseCode >= 400 then
    begin
      DeleteFile(ADestPath);
      raise ENetworkError.CreateFmt(
        'HTTP %d downloading %s', [ResponseCode, AUrl]);
    end;

    if AExpectedSHA256 <> '' then
      ValidateChecksum(ADestPath, AExpectedSHA256);
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

end.
