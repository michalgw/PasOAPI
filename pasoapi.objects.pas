{ PasOAPI

  Copyright (C) 2022 Michał Gawrycki (info.at.gmsystems.pl)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit PasOAPI.Objects;

{$mode objfpc}{$H+}{$M+}
{$interfaces corba}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, fpjson, Generics.Collections, fpjsonrtti;

const
  IPOStreamable_GUID = '{D9F5EBC6-17D1-418A-8735-2B21EED7E1AE}';

type

  { TPOReadOnlyAttribute }

  TPOReadOnlyAttribute = class(TCustomAttribute)
    constructor Create;
  end;

  { TPOTypeAttribute }

  TPOTypeAttribute = class(TCustomAttribute)
  private
    FValType: String;
  public
    constructor Create(const AType: String);
  published
    property ValType: String read FValType write FValType;
  end;

  { TPOFormatAttribute }

  TPOFormatAttribute = class(TCustomAttribute)
  private
    FFormat: String;
  public
    constructor Create(const AFormat: String);
  published
    property Format: String read FFormat write FFormat;
  end;

  { TPONameAttribute }

  TPONameAttribute = class(TCustomAttribute)
  private
    FValueName: String;
  public
    constructor Create(const AValName: String);
  published
    property ValueName: String read FValueName write FValueName;
  end;

  IPOStreamable = interface
    [IPOStreamable_GUID]
    function ToJSON: TJSONData;
    procedure FromJSON(AJSON: TJSONData);
  end;

  { TPOStreambleHelper }

  TPOStreambleHelper = type helper for IPOStreamable
    function ToJSONString: String;
    procedure FromJSONString(AData: String);
  end;

  { TPOObject }

  TPOObject = class(TObject, IPOStreamable)
  public
    constructor Create; virtual;
    function ToJSON: TJSONData;
    procedure FromJSON(AData: TJSONData);
  end;

  TPOObjectClass = class of TPOObject;

  { TPOBaseObjectList }

  TPOBaseObjectList = class(specialize TObjectList<TPOObject>, IPOStreamable)
  public
    class function ItemClass: TPOObjectClass; virtual;
    function AddNew: TPOObject;
    function ToJSON: TJSONData;
    procedure FromJSON(AJSON: TJSONData);
  end;

  { TPOObjectList }

  generic TPOObjectList<T: {TPO}TObject> = class(TPOBaseObjectList)
  private
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; const AValue: T);
  public
    class function ItemClass: TPOObjectClass; override;
    function AddNew: T;
    property Items[AIndex: SizeInt]: T read GetItem write SetItem;
  end;

  TPOOperation = (pooGet, pooPost, pooPut, pooDelete);
  TPOErrorEvent = procedure(Sender: TObject; AMessage: String) of object;

  { TPOConnector }

  TPOConnector = class
  private
    FAfterRequest: TNotifyEvent;
    FApiKey: String;
    FBasePath: String;
    FBeforeRequest: TNotifyEvent;
    FHost: String;
    FOnError: TPOErrorEvent;
  protected
    function DoRestGet(const APath: String; const AOperation: TPOOperation;
      const ARequest: String; out AResponse: String;
      const AContentType: String; const AHeaders: array of String): Integer; virtual; abstract;
    procedure DoBeforeRequest; virtual;
    procedure DoAfterRequest; virtual;
  public
    function RestGet(const APath: String; const AOperation: TPOOperation;
      const ARequest: String; out AResponse: String): Integer;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property Host: String read FHost write FHost;
    property BasePath: String read FBasePath write FBasePath;
    property OnError: TPOErrorEvent read FOnError write FOnError;
    property BeforeRequest: TNotifyEvent read FBeforeRequest write FBeforeRequest;
    property AfterRequest: TNotifyEvent read FAfterRequest write FAfterRequest;
  end;

  { TPOModule }

  TPOModule = class
  private
    FConnector: TPOConnector;
  protected
    function CreateQueryParams(AParams: array of const): String;
  public
  published
    property Connector: TPOConnector read FConnector write FConnector;
  end;

implementation

uses
  TypInfo, RttiUtils, Variants, Rtti, DateUtils, VarUtils, HTTPDefs;

{ TPOBaseObjectList }

class function TPOBaseObjectList.ItemClass: TPOObjectClass;
begin
  Result := TPOObject;
end;

function TPOBaseObjectList.AddNew: TPOObject;
begin
  Result := ItemClass.Create;
end;

function TPOBaseObjectList.ToJSON: TJSONData;
var
  El: TPOObject;
begin
  Result := TJSONArray.Create;
  for El in Self do
    TJSONArray(Result).Add(El.ToJSON);
end;

procedure TPOBaseObjectList.FromJSON(AJSON: TJSONData);
var
  El: TPOObject;
  I: Integer;
begin
  if (not Assigned(AJSON)) or (AJSON.JSONType <> jtArray) then
    Exit;
  for I := 0 to TJSONArray(AJSON).Count - 1 do
  begin
    El := ItemClass.Create;
    El.FromJSON(TJSONArray(AJSON).Objects[I]);
    Add(El);
  end;
end;

{ TPONameAttribute }

constructor TPONameAttribute.Create(const AValName: String);
begin
  FValueName := AValName;
end;

{ TPOConnector }

procedure TPOConnector.DoBeforeRequest;
begin
  if Assigned(FBeforeRequest) then
    FBeforeRequest(Self);
end;

procedure TPOConnector.DoAfterRequest;
begin
  if Assigned(FAfterRequest) then
    FAfterRequest(Self);
end;

function TPOConnector.RestGet(const APath: String;
  const AOperation: TPOOperation; const ARequest: String; out
  AResponse: String): Integer;
var
  Headers: array of String = ('accept: application/json');
begin
  DoBeforeRequest;
  if FApiKey <> '' then
    Headers := Concat(Headers, ['Authorization: Bearer' + FApiKey]);
  try
    Result := DoRestGet(FHost + FBasePath + APath, AOperation, ARequest,
      AResponse, 'application/json', Headers);
  except
    on E: Exception do
      if Assigned(FOnError) then
        FOnError(Self, E.Message);
  end;
  DoAfterRequest;
end;

{ TPOModule }

function TPOModule.CreateQueryParams(AParams: array of const): String;
const
  BOOLSTR: array[Boolean] of String = ('false', 'true');
var
  I: Integer;
  ParName, ParValue: String;
begin
  Result := '';
  if (Length(AParams) = 0) or (Length(AParams) mod 2 = 1) then
    Exit;
  for I := 0 to Length(AParams) div 2 do
  begin
    case AParams[I * 2].VType of
      vtChar: ParName := AParams[I * 2].VChar;
      vtWideChar: ParName := AParams[I * 2].VWideChar;
      vtString: ParName := AParams[I * 2].VString^;
      vtPChar: ParName := AParams[I * 2].VPChar;
      vtPWideChar: ParName := AParams[I * 2].VPWideChar;
      vtAnsiString: ParName := AnsiString(AParams[I * 2].VAnsiString);
      vtWideString: ParName := WideString(AParams[I * 2].VWideString);
      else
        Continue;
    end;
    case AParams[I * 2 + 1].VType of
      vtInteger: ParValue := IntToStr(AParams[I * 2 + 1].VInteger);
      vtBoolean: ParValue := BOOLSTR[AParams[I * 2 + 1].VBoolean];
      vtChar: ParValue := AParams[I * 2 + 1].VChar;
      vtWideChar: ParValue := AParams[I * 2 + 1].VWideChar;
      vtExtended: ParValue := FloatToStr(AParams[I * 2 + 1].VExtended^);
      vtString: ParValue := AParams[I * 2 + 1].VString^;
      vtPChar: ParValue := AParams[I * 2 + 1].VPChar;
      vtPWideChar: ParValue := AParams[I * 2 + 1].VPWideChar;
      vtAnsiString: ParValue := AnsiString(AParams[I * 2 + 1].VAnsiString);
      vtCurrency: ParValue := FloatToStr(AParams[I * 2 + 1].VCurrency^);
      vtVariant: ParValue := VarToStr(AParams[I * 2 + 1].VVariant^);
      vtWideString: ParValue := WideString(AParams[I * 2 + 1].VWideString);
      vtInt64: ParValue := IntToStr(AParams[I * 2 + 1].VInt64^);
      vtQWord: ParValue := IntToStr(AParams[I * 2 + 1].VQWord^);
      else
        Continue;
    end;
    if Result = '' then
      Result := '?'
    else
      Result := Result + '&';
    Result := Result + ParName + '=' + HTTPEncode(ParValue);
  end;
end;

{ TPOStreambleHelper }

function TPOStreambleHelper.ToJSONString: String;
var
  J: TJSONData;
begin
  J := Self.ToJSON;
  Result := J.AsJSON;
  J.Free;
end;

procedure TPOStreambleHelper.FromJSONString(AData: String);
var
  J: TJSONData;
begin
  J := GetJSON(AData);
  Self.FromJSON(J);
  J.Free;
end;

{ TPOReadOnlyAttribute }

constructor TPOReadOnlyAttribute.Create;
begin

end;

{ TPOObjectList }

function TPOObjectList.AddNew: T;
begin
  Result := T(inherited AddNew);
end;

function TPOObjectList.GetItem(AIndex: SizeInt): T;
begin
  Result := T(inherited Items[AIndex]);
end;

procedure TPOObjectList.SetItem(AIndex: SizeInt; const AValue: T);
begin
  inherited Items[AIndex] := AValue;
end;

class function TPOObjectList.ItemClass: TPOObjectClass;
begin
  Result := T;
end;

{ TPOFormatAttribute }

constructor TPOFormatAttribute.Create(const AFormat: String);
begin
  FFormat := AFormat;
end;

{ TPOTypeAttribute }

constructor TPOTypeAttribute.Create(const AType: String);
begin
  FValType := AType;
end;

{ TPOObject }

constructor TPOObject.Create;
begin

end;

function MapToJSON(Avalue: Variant; const AType, AFormat: String): TJSONData;
begin
  Result := nil;
  if VarIsNull(Avalue) then
    Result := TJSONNull.Create
  else
    case AType of
      'string': case AFormat of
        'date-time': begin
          if VarIsNumeric(Avalue) or VarIsType(Avalue, vardate) then
            Result := TJSONString.Create(DateToISO8601(Avalue))
          else if VarIsStr(Avalue) then
            Result := TJSONString.Create(Avalue);
        end;
        else
          if VarIsStr(Avalue) then
            Result := TJSONString.Create(Avalue);
      end;
      'integer': case AFormat of
        'int32', 'int64': begin
          if VarIsNumeric(Avalue) then
            Result := TJSONInt64Number.Create(Avalue);
        end;
      end;
      'boolean': begin
        if VarIsBool(Avalue) then
          Result := TJSONBoolean.Create(Avalue)
        else if VarIsNumeric(Avalue) then
          Result := TJSONBoolean.Create(Avalue <> 0);
      end;
      'number': if VarIsNumeric(Avalue) then begin
        Result := TJSONFloatNumber.Create(Avalue);
      end;
    end;
  if Result = nil then
    Result := TJSONNull.Create;
end;

function MapToVariant(AData: TJSONData; const AType, AFormat: String): Variant;
var
  TmpD: TDateTime;
begin
  Result := Null;
  case AType of
    'string': case AFormat of
      'date-time': case AData.JSONType of
        jtString: if TryISO8601ToDate(AData.AsString, TmpD) then
          Result := TmpD;
        jtNumber: Result := TDateTime(AData.AsFloat);
        else
      end;
      else
        if not (AData.JSONType in [jtNull, jtObject, jtArray]) then
          Result := AData.AsString;
    end;
    'integer': begin
      if AData.JSONType = jtNumber then
        case AFormat of
          'int32': Result := AData.AsInteger;
          else
            Result := AData.AsInt64;
        end;
    end;
    'boolean': if AData.JSONType <> jtNull then
      Result := AData.AsBoolean;
    'number': if AData.JSONType = jtNumber then
      case AFormat of
        'float': Result := Single(AData.AsFloat);
        else
          Result := AData.AsFloat;
      end;
  end;
end;

function TPOObject.ToJSON: TJSONData;
var
  TiContext: TRttiContext;
  TiType: TRttiType;
  TiProp: TRttiProperty;
  TiAttr: TCustomAttribute;
  AttrRO: Boolean;
  AttrType, AttrFormat: String;
  Value: TValue;
  VarVal: Variant;
  ValueName: String;
begin
  Result := TJSONObject.Create;
  TiContext := TRttiContext.Create;
  TiType := TiContext.GetType(Self.ClassType);
  for TiProp in TiType.GetProperties do
  begin
    AttrRO := False;
    AttrType := '';
    AttrFormat := '';

    ValueName := TiProp.Name;

    for TiAttr in TiProp.GetAttributes do
    begin
      if TiAttr is TPOReadOnlyAttribute then
        AttrRO := True;
      if TiAttr is TPOTypeAttribute then
        AttrType := TPOTypeAttribute(TiAttr).ValType;
      if TiAttr is TPOFormatAttribute then
        AttrFormat := TPOFormatAttribute(TiAttr).Format;
      if TiAttr is TPONameAttribute then
        ValueName := TPONameAttribute(TiAttr).ValueName;
    end;

    if AttrRO then
      Continue;

    case TiProp.PropertyType.TypeKind of
      tkClass: begin
        Value := TiProp.GetValue(Self);
        if Value.IsObject and Assigned(Value.AsObject)
          and Supports(Value.AsObject, IPOStreamable_GUID) then
          TJSONObject(Result).Add(ValueName, (Value.AsObject as IPOStreamable).ToJSON)
        else
          TJSONObject(Result).Add(ValueName);
      end;
      tkVariant: begin
        VarVal := GetVariantProp(Self, TiProp.Name);
        TJSONObject(Result).Add(ValueName, MapToJSON(VarVal, AttrType, AttrFormat));
      end;
      else
    end;
  end;
  TiContext.Free;
end;

procedure TPOObject.FromJSON(AData: TJSONData);
var
  I: Integer;
  TiContext: TRttiContext;
  TiType: TRttiType;
  TiProp: TRttiProperty;
  TiAttr: TCustomAttribute;
  AttrType, AttrFormat: String;
  Value: TValue;
begin
  if AData is TJSONObject then
  begin
    TiContext := TRttiContext.Create;
    TiType := TiContext.GetType(Self.ClassType);
    for I := 0 to TJSONObject(AData).Count - 1 do
    begin
      TiProp := TiType.GetProperty(TJSONObject(AData).Names[I]);

      if TiProp = nil then
        Continue;

      AttrType := '';
      AttrFormat := '';
      for TiAttr in TiProp.GetAttributes do
      begin
        if TiAttr is TPOTypeAttribute then
          AttrType := TPOTypeAttribute(TiAttr).ValType;
        if TiAttr is TPOFormatAttribute then
          AttrFormat := TPOFormatAttribute(TiAttr).Format;
      end;

      case TiProp.PropertyType.TypeKind of
        tkVariant: SetVariantProp(Self, TiProp.Name, MapToVariant(AData.Items[I], AttrType, AttrFormat));
        tkClass: begin
          Value := TiProp.GetValue(Self);
          if Value.IsObject and Assigned(Value.AsObject)
            and Supports(Value.AsObject, IPOStreamable_GUID) then
            (Value.AsObject as IPOStreamable).FromJSON(AData.Items[I]);
        end;
        else
      end;
    end;
    TiContext.Free;
  end;
end;

end.

