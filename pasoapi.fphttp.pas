unit PasOAPI.FPHttp;

{$mode objfpc}{$H+}
{$define USE_SSL}

interface

uses
  Classes, SysUtils, fphttpclient, PasOAPI.Objects
  {$ifdef USE_SSL}, openssl, opensslsockets{$endif};

type

  { TPOFPHTTPConnector }

  TPOFPHTTPConnector = class(TPOConnector)
  protected
    function DoRestGet(const APath: String; const AOperation: TPOOperation;
      const ARequest: String; out AResponse: String;
      const AContentType: String; const AHeaders: array of String): Integer;
      override;
  end;

implementation

uses
  httpprotocol;

{ TPOFPHTTPConnector }

function TPOFPHTTPConnector.DoRestGet(const APath: String;
  const AOperation: TPOOperation; const ARequest: String; out
  AResponse: String; const AContentType: String; const AHeaders: array of String
  ): Integer;
const
  FPCHTTPMETHOD: array[TPOOperation] of String = ('GET', 'POST', 'PUT', 'DELETE');
var
  Request: TStringStream = nil;
  Response: TStringStream = nil;
begin
  with TFPHTTPClient.Create(nil) do
  try
    if AContentType <> '' then
      AddHeader(HeaderContentType, AContentType);
    if Length(AHeaders) > 0 then
      RequestHeaders.AddStrings(AHeaders);
    if ARequest <> '' then
    begin
      Request := TStringStream.Create(ARequest);
      RequestBody := Request;
    end;
    Response := TStringStream.Create;
    HTTPMethod(FPCHTTPMETHOD[AOperation], APath, Response, []);
    AResponse := Response.DataString;
    Result := ResponseStatusCode;
  finally
    if Assigned(Response) then
      Response.Free;
    if Assigned(Request) then
      Request.Free;
    Free;
  end;
end;

{$ifdef USE_SSL}
initialization
  InitSSLInterface;
{$endif}

end.

