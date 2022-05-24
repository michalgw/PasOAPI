{ PasOAPIImp

  Copyright (C) 2022 Michał Gawrycki (info.at.gmsystems.pl)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}


unit uFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ActnList, DBGrids, ExtCtrls, StdCtrls, SynEdit, SynHighlighterPas;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionLoadProject: TAction;
    ActionSaveProject: TAction;
    ActionDefinitionReplace: TAction;
    ActionGenerate: TAction;
    ActionOpenSchema: TAction;
    ActionList1: TActionList;
    DBGrid3: TDBGrid;
    DBGrid4: TDBGrid;
    DBGrid5: TDBGrid;
    DBGrid6: TDBGrid;
    DBGrid7: TDBGrid;
    DS_DTags: TDataSource;
    DS_DPaths: TDataSource;
    DS_DOperations: TDataSource;
    DS_DParams: TDataSource;
    DS_DResponses: TDataSource;
    DOperations: TBufDataset;
    DResponses: TBufDataset;
    DParams: TBufDataset;
    DPaths: TBufDataset;
    DTags: TBufDataset;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DS_DDefinitionFields: TDataSource;
    DS_DDefinitions: TDataSource;
    DDefinitionFields: TBufDataset;
    DDefinitions: TBufDataset;
    EditBaseURL: TEdit;
    Label1: TLabel;
    MemoDebug: TMemo;
    OpenDialogProj: TOpenDialog;
    OpenDialogSchema: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    ReplaceDialog1: TReplaceDialog;
    SaveDialogProj: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    StatusBar1: TStatusBar;
    SynEditOut: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    TabSheetPaths: TTabSheet;
    TabSheetGeneral: TTabSheet;
    TabSheetDef: TTabSheet;
    TabSheetOut: TTabSheet;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ActionDefinitionReplaceExecute(Sender: TObject);
    procedure ActionGenerateExecute(Sender: TObject);
    procedure ActionLoadProjectExecute(Sender: TObject);
    procedure ActionOpenSchemaExecute(Sender: TObject);
    procedure ActionSaveProjectExecute(Sender: TObject);
    procedure DS_DDefinitionsDataChange(Sender: TObject; Field: TField);
    procedure DS_DOperationsDataChange(Sender: TObject; Field: TField);
    procedure DS_DPathsDataChange(Sender: TObject; Field: TField);
    procedure DS_DTagsDataChange(Sender: TObject; Field: TField);
    procedure FormShow(Sender: TObject);
    procedure ReplaceDialog1FindDef(Sender: TObject);
    procedure ReplaceDialog1ReplaceDef(Sender: TObject);
  private

  public
    //function DefinitionLookup
    function DefinitionLookup(ARef: String; AField: String): Variant;
    function DefinitionLookupPasName(ARef: String): String;
    function DefinitionLookupId(ARef: String): Integer;
    procedure DefinitionLookupIdPasName(ARef: String; out AID: Integer; out APasName: String);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  fpjson, Variants;

//function GetPasType(AType, AFormat: String): String;
//begin
//  case LowerCase(AType) of
//    'string': case LowerCase(AFormat) of
//      'date':
//    end;
//  end;
//end;

function PascalizeName(AName: String): String; inline;
begin
  Result := 'T' + StringReplace(AName, '.', '_', [rfReplaceAll]);
end;

{ TFormMain }

procedure TFormMain.ActionOpenSchemaExecute(Sender: TObject);

var
  TagId: Integer;

procedure ParseDefinitions(AData: TJSONData);
var
  I, J: Integer;
  JA: TJSONData;
  JD: TJSONObject;
  JF: TJSONObject;
begin
  JA := AData.FindPath('definitions');
  for I := 0 to JA.Count - 1 do
  begin
    JD := TJSONObject(TJSONArray(JA).Items[I]);
    if JD <> nil then
    begin
      DDefinitions.Append;
      DDefinitions['Id'] := I;
      DDefinitions['Active'] := True;
      DDefinitions['Name'] := TJSONObject(JA).Names[I];
      DDefinitions['PasName'] := PascalizeName(DDefinitions['Name']);
      if JD.Find('type') <> nil then
        DDefinitions['Type'] := JD.Strings['type'];
      DDefinitions.Post;
      if JD.Find('properties') <> nil then
        for J := 0 to JD.Objects['properties'].Count - 1 do
        begin
          JF := TJSONObject(JD.Objects['properties'].Items[J]);
          DDefinitionFields.Append;
          DDefinitionFields['DefinitionId'] := I;
          DDefinitionFields['Active'] := True;
          DDefinitionFields['Name'] := JD.Objects['properties'].Names[J];
          if JF.Find('type') <> nil then
            DDefinitionFields['Type'] := JF.Strings['type'];
          if JF.Find('format') <> nil then
            DDefinitionFields['Format'] := JF.Strings['format'];
          if JF.Find('$ref') <> nil then
            DDefinitionFields['Reference'] := JF.Strings['$ref'];
          if JF.Find('description') <> nil then
            DDefinitionFields['Description'] := JF.Strings['description'];
          if JF.Find('readOnly') <> nil then
            DDefinitionFields['ReadOnly'] := JF.Booleans['readOnly'];
          if (not DDefinitionFields.FieldByName('Type').IsNull)
            and (DDefinitionFields.FieldByName('Type').AsString = 'array')
            and (JF.Find('items') <> nil) then
          begin
            if TJSONObject(JF.Objects['items']).Find('type') <> nil then
              DDefinitionFields['ArrayType'] := TJSONObject(JF.Objects['items']).Strings['type'];
            if TJSONObject(JF.Objects['items']).Find('format') <> nil then
              DDefinitionFields['ArrayFormat'] := TJSONObject(JF.Objects['items']).Strings['format'];
            if TJSONObject(JF.Objects['items']).Find('$ref') <> nil then
              DDefinitionFields['ArrayReference'] := TJSONObject(JF.Objects['items']).Strings['$ref'];
          end;
          DDefinitionFields.Post;
        end;
    end;
  end;
end;

function PathsAddTag(ATag: String): Integer;
var
  V: Variant;
begin
  V := DTags.Lookup('Name', ATag, 'Id');
  if VarIsNumeric(V) then
    Result := V
  else
  begin
    Inc(TagId);
    DTags.Append;
    DTags['Id'] := TagId;
    DTags['Name'] := ATag;
    DTags['Active'] := True;
    DTags['PasName'] := 'T' + ATag;
    DTags.Post;
    Result := TagId;
  end;
end;

procedure ParsePaths(AData: TJSONData);
var
  JP: TJSONObject;
  IP, IO, IPr: Integer;
  JOper, JResp, JSch: TJSONObject;
  JArr: TJSONArray;
  ActTagId: Integer;
  PathId: Integer;
  OperId: Integer;
begin
  if (AData.FindPath('paths') <> nil) and (AData.FindPath('paths').JSONType = jtObject) then
  begin
    TagId := 0;
    PathId := 0;
    OperId := 0;
    JP := TJSONObject(AData.FindPath('paths'));
    for IP := 0 to JP.Count - 1 do
    begin
      JOper := TJSONObject(JP.Items[IP]);
      if (TJSONObject(JOper.Items[0]).Find('tags') <> nil) and (TJSONObject(JOper.Items[0]).Arrays['tags'].Count > 0) then
        ActTagId := PathsAddTag(TJSONObject(JOper.Items[0]).Arrays['tags'].Strings[0]);
      DPaths.Append;
      DPaths['TagId'] := ActTagId;
      DPaths['Id'] := PathId;
      DPaths['Path'] := JP.Names[IP];
      DPaths['Active'] := True;
      DPaths.Post;

      for IO := 0 to JOper.Count - 1 do
      begin
        DOperations.Append;
        DOperations['PathId'] := PathId;
        DOperations['Id'] := OperId;
        DOperations['Active'] := True;
        DOperations['Operation'] := JOper.Names[IO];
        if TJSONObject(JOper.Items[IO]).Find('summary') <> nil then
          DOperations['Summary'] := TJSONObject(JOper.Items[IO]).Strings['summary'];
        if TJSONObject(JOper.Items[IO]).Find('operationId') <> nil then
          DOperations['OperationId'] := TJSONObject(JOper.Items[IO]).Strings['operationId'];
        DOperations.Post;

        if TJSONObject(JOper.Items[IO]).Find('parameters', JArr) then
          for IPr := 0 to JArr.Count - 1 do
          begin
            DParams.Append;
            DParams['OperationId'] := OperId;
            DParams['Active'] := True;
            if JArr.Objects[IPr].Find('name') <> nil then
              DParams['Name'] := JArr.Objects[IPr].Strings['name'];
            if JArr.Objects[IPr].Find('required') <> nil then
              DParams['Required'] := JArr.Objects[IPr].Booleans['required'];
            if JArr.Objects[IPr].Find('in') <> nil then
              DParams['In'] := JArr.Objects[IPr].Strings['in'];
            if JArr.Objects[IPr].Find('type') <> nil then
              DParams['Type'] := JArr.Objects[IPr].Strings['type'];
            if JArr.Objects[IPr].Find('format') <> nil then
              DParams['Format'] := JArr.Objects[IPr].Strings['format'];
            if JArr.Objects[IPr].FindPath('schema.$ref') <> nil then
              DParams['Reference'] := JArr.Objects[IPr].FindPath('schema.$ref').AsString;
            DParams.Post;
          end;

        if TJSONObject(JOper.Items[IO]).Find('responses', JResp) then
          for IPr := 0 to JResp.Count - 1 do
          begin
            DResponses.Append;
            DResponses['OperationId'] := OperId;
            DResponses['Active'] := True;
            DResponses['Response'] := JResp.Names[IPr];
            if TJSONObject(JResp.Items[IPr]).Find('description') <> nil then
              DResponses['Description'] := TJSONObject(JResp.Items[IPr]).Strings['description'];
            if TJSONObject(JResp.Items[IPr]).Find('schema', JSch) then
            begin
              if JSch.Find('type') <> nil then
                DResponses['Type'] := JSch.Strings['type'];
              if JSch.Find('format') <> nil then
                DResponses['Format'] := JSch.Strings['format'];
              if JSch.Find('$ref') <> nil then
                DResponses['Reference'] := JSch.Strings['$ref'];
              if JSch.Find('items') <> nil then
                with TJSONObject(JSch.Find('items')) do
                begin
                  if Find('type') <> nil then
                    DResponses['ArrayType'] := Strings['type'];
                  if Find('format') <> nil then
                    DResponses['ArrayFormat'] := Strings['format'];
                  if Find('$ref') <> nil then
                    DResponses['ArrayReference'] := Strings['$ref'];
                end;
            end;
            DResponses.Post;
          end;

        Inc(OperId);
      end;

      Inc(PathId);
    end;
  end;
end;

var
  FS: TFileStream;
  JSchema: TJSONObject;
begin
  FS := nil;
  JSchema := nil;
  if OpenDialogSchema.Execute then
  begin
    try
      FS := TFileStream.Create(OpenDialogSchema.FileName, fmOpenRead);
      JSchema := TJSONObject(GetJSON(FS));
    finally
      if Assigned(FS) then
        FS.Free;
    end;

    if not Assigned(JSchema) then
      Exit;

    DDefinitions.DisableControls;
    DDefinitionFields.DisableControls;
    DTags.DisableControls;
    DPaths.DisableControls;
    DOperations.DisableControls;
    DParams.DisableControls;
    DResponses.DisableControls;
    try
      if JSchema.Find('basePath') <> nil then
        EditBaseURL.Text := JSchema.Strings['basePath'];
      ParseDefinitions(JSchema);
      ParsePaths(JSchema);
    finally
    end;
    DDefinitions.EnableControls;
    DDefinitionFields.EnableControls;
    DTags.EnableControls;
    DPaths.EnableControls;
    DOperations.EnableControls;
    DParams.EnableControls;
    DResponses.EnableControls;
    JSchema.Free;
  end;
end;

procedure TFormMain.ActionSaveProjectExecute(Sender: TObject);
var
  FS: TFileStream;
  MS: TMemoryStream;
begin
  if SaveDialogProj.Execute then
  begin
    FS := TFileStream.Create(SaveDialogProj.FileName, fmCreate);
    MS := TMemoryStream.Create;

    DDefinitions.SaveToStream(MS);
    FS.WriteQWord(MS.Size);
    MS.Position := 0;
    FS.Write(MS.Memory, MS.Size);
    MS.Clear;

    DDefinitionFields.SaveToStream(MS);
    FS.WriteQWord(MS.Size);
    MS.Position := 0;
    FS.Write(MS.Memory, MS.Size);
    MS.Clear;

    DTags.SaveToStream(MS);
    FS.WriteQWord(MS.Size);
    MS.Position := 0;
    FS.Write(MS.Memory, MS.Size);
    MS.Clear;

    DPaths.SaveToStream(MS);
    FS.WriteQWord(MS.Size);
    MS.Position := 0;
    FS.Write(MS.Memory, MS.Size);
    MS.Clear;

    DOperations.SaveToStream(MS);
    FS.WriteQWord(MS.Size);
    MS.Position := 0;
    FS.Write(MS.Memory, MS.Size);
    MS.Clear;

    DParams.SaveToStream(MS);
    FS.WriteQWord(MS.Size);
    MS.Position := 0;
    FS.Write(MS.Memory, MS.Size);
    MS.Clear;

    DResponses.SaveToStream(MS);
    FS.WriteQWord(MS.Size);
    MS.Position := 0;
    FS.Write(MS.Memory, MS.Size);
    MS.Clear;

    MS.Free;
    FS.Free;
  end;
end;

procedure TFormMain.ActionGenerateExecute(Sender: TObject);
var
  STypes: TStringList;
  SImpl: TStringList;
  SClsCrt, SClsDst, SClsPrp, SClsPrpP: TStringList;
  SFwd: TStringList;
  S, StTmp1, StTmp2: String;
  I: Integer;
  ParTyp: (ptNone, ptQuery, ptRequest);
  ParObj: String;
  ResTyp: (rtNone, rtString, rtObject, rtValArray, rtObjArray);
  ResObj: String;

procedure UpdateForwards(AType: String; AID: Integer);
begin
  if DDefinitions['Id'] < AID then
    if SFwd.IndexOf(AType) < 0 then
      SFwd.Add(AType);
end;

begin
  if DDefinitions.RecordCount = 0 then
    Exit;

  SynEditOut.Clear;

  STypes := TStringList.Create;
  SImpl := TStringList.Create;
  SClsCrt := TStringList.Create;
  SClsDst := TStringList.Create;
  SClsPrp := TStringList.Create;
  SClsPrpP := TStringList.Create;
  SFwd := TStringList.Create;
  SFwd.Duplicates := dupIgnore;

  //DDefinitions.DisableControls;
  //DDefinitionFields.DisableControls;

  DDefinitions.First;
  while not DDefinitions.EOF do
  begin
    if DDefinitions['Active'] then
    begin
      SClsCrt.Clear;
      SClsDst.Clear;
      SClsPrp.Clear;
      SClsPrpP.Clear;

      DDefinitionFields.First;
      while not DDefinitionFields.EOF do
      begin
        if DDefinitionFields['Active'] then
        begin
          if DDefinitionFields.FieldByName('Reference').AsString <> '' then
          begin
            DefinitionLookupIdPasName(DDefinitionFields.FieldByName('Reference').AsString, I, S);
            UpdateForwards(S, I);
            SClsPrp.Append('    F' + DDefinitionFields['Name'] + ': ' + S + ';');
            SClsPrpP.Append('    property ' + DDefinitionFields['Name'] + ': ' + S
              + ' read F' + DDefinitionFields['Name'] + ' write F' + DDefinitionFields['Name'] + ';');
            SClsCrt.Append('  F' + DDefinitionFields['Name'] + ' := ' + S + '.Create;');
            SClsDst.Append('  if Assigned(F' + DDefinitionFields['Name'] + ') then');
            SClsDst.Append('    F' + DDefinitionFields['Name'] + '.Free;');
          end
          else if DDefinitionFields['Type'] = 'array' then
          begin
            if DDefinitionFields.FieldByName('ArrayReference').AsString <> '' then
            begin
              DefinitionLookupIdPasName(DDefinitionFields.FieldByName('ArrayReference').AsString, I, S);
              UpdateForwards(S, I);
              SClsPrp.Append('    F' + DDefinitionFields['Name'] + ': specialize TPOObjectList<' + S + '>;');
              SClsPrpP.Append('    property ' + DDefinitionFields['Name'] + ': specialize TPOObjectList<' + S
                + '> read F' + DDefinitionFields['Name'] + ' write F' + DDefinitionFields['Name'] + ';');
              SClsCrt.Append('  F' + DDefinitionFields['Name'] + ' := specialize TPOObjectList<' + S + '>.Create(True);');
              SClsDst.Append('  if Assigned(F' + DDefinitionFields['Name'] + ') then');
              SClsDst.Append('    F' + DDefinitionFields['Name'] + '.Free;');
            end
            else
            begin
              SClsPrp.Append('    F' + DDefinitionFields['Name'] + ': specialize TList<Variant>;');
              SClsPrpP.Append('    property ' + DDefinitionFields['Name'] + ': specialize TList<Variant> read F'
                + DDefinitionFields['Name'] + ' write F' + DDefinitionFields['Name'] + ';');
              SClsCrt.Append('  F' + DDefinitionFields['Name'] + ' := specialize TList<' + S + '>.Create;');
              SClsDst.Append('  if Assigned(F' + DDefinitionFields['Name'] + ') then');
              SClsDst.Append('    F' + DDefinitionFields['Name'] + '.Free;');
            end;
          end
          else
          begin
            SClsPrp.Append('    F' + DDefinitionFields['Name'] + ': Variant;');
            if (not DDefinitionFields.FieldByName('ReadOnly').IsNull) and
              DDefinitionFields.FieldByName('ReadOnly').AsBoolean then
              SClsPrpP.Append('    [TPOReadOnlyAttribute]');
            if (not DDefinitionFields.FieldByName('Type').IsNull) and (DDefinitionFields['Type'] <> '') then
              SClsPrpP.Append('    [TPOTypeAttribute(''' + DDefinitionFields['Type'] + ''')]');
            if (not DDefinitionFields.FieldByName('Format').IsNull) and (DDefinitionFields['Format'] <> '') then
              SClsPrpP.Append('    [TPOFormatAttribute(''' + DDefinitionFields['Format'] + ''')]');
            SClsPrpP.Append('    property ' + DDefinitionFields['Name'] + ': Variant read F'
              + DDefinitionFields['Name'] + ' write F' + DDefinitionFields['Name'] + ';');
          end;
        end;
        DDefinitionFields.Next;
      end;

      STypes.Append('  { ' + DDefinitions['PasName'] + ' }');
      STypes.Append('');
      STypes.Append('  ' + DDefinitions['PasName'] + ' = class(TPOObject)');
      STypes.Append('  private');
      STypes.AddStrings(SClsPrp);
      if SClsCrt.Count > 0 then
      begin;
        STypes.Append('  public');
        STypes.Append('    constructor Create; override;');
        STypes.Append('    destructor Destroy; override;');
      end;
      STypes.Append('  published');
      STypes.AddStrings(SClsPrpP);
      STypes.Append('  end;');
      STypes.Append('');

      if SClsCrt.Count > 0 then
      begin
        SImpl.Append('{ ' + DDefinitions['PasName'] + ' }');
        SImpl.Append('');
        SImpl.Append('constructor ' + DDefinitions['PasName'] + '.Create;');
        SImpl.Append('begin');
        SImpl.Append('  inherited;');
        SImpl.AddStrings(SClsCrt);
        SImpl.Append('end;');
        SImpl.Append('');
        SImpl.Append('destructor ' + DDefinitions['PasName'] + '.Destroy;');
        SImpl.Append('begin');
        SImpl.AddStrings(SClsDst);
        SImpl.Append('  inherited;');
        SImpl.Append('end;');
        SImpl.Append('');
      end;
    end;
    DDefinitions.Next;
  end;

  DTags.First;
  while not DTags.EOF do
  begin
    if DTags['Active'] then
    begin
      SClsPrp.Clear;
      SClsPrpP.Clear;

      STypes.Add('  { ' + DTags['PasName'] + ' }');
      STypes.Add('');
      STypes.Add('  ' + DTags['PasName'] + ' = class(TPOModule)');
      STypes.Add('  public');

      SImpl.Add('{ ' + DTags['PasName'] + ' }');
      SImpl.Add('');

      DPaths.First;
      while not DPaths.EOF do
      begin
        if DPaths['Active'] then
        begin
          DOperations.First;
          while not DOperations.EOF do
          begin
            SClsCrt.Clear;
            SClsDst.Clear;
            StTmp1 := '';
            StTmp2 := '';
            ParTyp := ptNone;
            ParObj := '';
            ResTyp := rtNone;
            ResObj := '';
            DParams.First;



            while not DParams.EOF do
            begin
              if DParams['Active'] then
              begin
                if (DParams['In'] = 'body') and (not DParams.FieldByName('Reference').IsNull)
                  and (DParams.FieldByName('Reference').AsString <> '') then
                begin
                  ParTyp := ptRequest;
                  ParObj := DefinitionLookupPasName(DParams.FieldByName('Reference').AsString);
                end
                else if DParams['In'] = 'query' then
                  ParTyp := ptQuery;
              end;
              DParams.Next;
            end;

            DResponses.First;
            while not DResponses.EOF do
            begin
              ResTyp := rtNone;
              ResObj := '';
              if DResponses['Active'] then
              begin
                if DResponses['Type'] = 'array' then
                begin
                  if (not DResponses.FieldByName('ArrayReference').IsNull) and
                    (DResponses.FieldByName('ArrayReference').AsString <> '') then
                  begin
                    ResTyp := rtObjArray;
                    ResObj := 'specialize TPOObjectList<' + DefinitionLookupPasName(DResponses.FieldByName('ArrayReference').AsString) +
                      '>';
                  end;
                end
                else if DResponses['Type'] = 'string' then
                begin
                  ResTyp := rtString;
                end
                else if (not DResponses.FieldByName('Reference').IsNull) and
                  (DResponses.FieldByName('Reference').AsString <> '') then
                begin
                  ResTyp := rtObject;
                  ResObj := DefinitionLookupPasName(DResponses.FieldByName('Reference').AsString);
                end;
                SClsCrt.Add('    ' + IntToStr(DResponses['Response']) + ': begin');
                case ResTyp of
                  rtObject: begin
                    SClsCrt.Add('      AResponse := ' + ResObj + '.Create;');
                    SClsCrt.Add('      IPOStreamable(AResponse).FromJSONString(Response);');
                  end;
                  rtObjArray: begin
                    SClsCrt.Add('      AResult := ' + ResObj + '.Create(True);');
                    SClsCrt.Add('      IPOStreamable(AResponse).FromJSONString(Response);');
                  end;
                  rtString: begin
                    SClsCrt.Add('      AResponse := Response;');
                  end;
                end;
                SClsCrt.Add('    end;');
              end;
              DResponses.Next;
            end;

            StTmp1 := '';
            if ParTyp = ptQuery then
              StTmp1 := 'AParams: array of const'
            else if ParTyp = ptRequest then
              StTmp1 := 'AParams: ' + ParObj;
            if ResTyp in [rtObject, rtObjArray, rtString] then
            begin
              if ParTyp in [ptQuery, ptRequest] then
                StTmp1 := StTmp1 + '; ';
              if ResTyp in [rtObject, rtObjArray] then
                StTmp1 := StTmp1 + 'out AResponse: ' + ResObj
              else StTmp1 := StTmp1 + 'out AResponse: String';
            end;

            STypes.Add('    function ' + DOperations['OperationId'] + '(' + StTmp1 + '): Integer;');

            SImpl.Add('function ' + DTags['PasName'] + '.' + DOperations['OperationId'] + '(' + StTmp1 + '): Integer;');
            SImpl.Add('var');
            SImpl.Add('  Response: String;');
            SImpl.Add('begin');
            StTmp2 :='  Result := Connector.RestGet(' + QuotedStr(DPaths['Path']);
            if ParTyp = ptQuery then
              StTmp2 := StTmp2 + ' + CreateQueryParams(AParams)';
            StTmp2 := StTmp2 + ', poo' + DOperations['Operation'] + ', ';
            if ParTyp = ptRequest then
              StTmp2 := StTmp2 + 'IPOStreamable(AParams).ToJSONString'
            else
              StTmp2 := StTmp2 + QuotedStr('');
            StTmp2 := StTmp2 + ', Response);';
            SImpl.Add(StTmp2);
            SImpl.Add('  case Result of');
            SImpl.AddStrings(SClsCrt);
            SImpl.Add('  end;');
            SImpl.Add('end;');
            SImpl.Add('');
            DOperations.Next;
          end;
        end;
        DPaths.Next;
      end;
      STypes.Add('  end;');
      STypes.Add('');
    end;
    DTags.Next;
  end;
  //SynEditOut.Text := 'unit import;' + LineEnding + LineEnding + 'interface'
  //  + LineEnding + LineEnding + 'uses' + LineEnding + '  PasOAPI.Objects;'
  //  + LineEnding + LineEnding + 'type' + LineEnding + STypes.Text + 'implementation'
  //  + LineEnding + SImpl.Text + 'end.';
  SynEditOut.Append('unit ImportLib;');
  SynEditOut.Append('{$MODE OBJFPC}{$H+}{$M+}{$modeswitch prefixedattributes}');
  SynEditOut.Append('interface');
  SynEditOut.Append('');
  SynEditOut.Append('uses');
  SynEditOut.Append('  PasOAPI.Objects, Generics.Collections;');
  SynEditOut.Append('');
  SynEditOut.Append('type');
  if SFwd.Count > 0 then
  begin
    SynEditOut.Append('  { Forward definitions }');
    for S in SFwd do
      SynEditOut.Append('  ' + S + ' = class;');
    SynEditOut.Append('');
  end;
  SynEditOut.Lines.AddStrings(STypes);
  SynEditOut.Append('implementation');
  SynEditOut.Append('');
  SynEditOut.Lines.AddStrings(SImpl);
  SynEditOut.Append('end.');

  STypes.Free;
  SImpl.Free;
  SClsCrt.Free;
  SClsDst.Free;
  SClsPrp.Free;
  SClsPrpP.Free;
  SFwd.Free;
end;

procedure TFormMain.ActionLoadProjectExecute(Sender: TObject);
var
  FS: TFileStream;
  MS: TMemoryStream;
  Q: QWord;
  P: Pointer;
begin
  if OpenDialogProj.Execute then
  begin
    DDefinitions.Close;
    DDefinitionFields.Close;
    DTags.Close;
    DPaths.Close;
    DOperations.Close;
    DParams.Close;
    DResponses.Close;

    FS := TFileStream.Create(OpenDialogProj.FileName, fmOpenRead + fmShareDenyWrite);
    MS := TMemoryStream.Create;

    Q := FS.ReadQWord;
    MS.SetSize(Q);
    P := MS.Memory;
    FS.Read(P^, Q);
    MS.Position := 0;
    DDefinitions.LoadFromStream(MS, dfBinary);

    Q := FS.ReadQWord;
    MS.SetSize(Q);
    P := MS.Memory;
    FS.Read(P^, Q);
    MS.Position := 0;
    DDefinitionFields.LoadFromStream(MS, dfBinary);

    Q := FS.ReadQWord;
    MS.SetSize(Q);
    P := MS.Memory;
    FS.Read(P^, Q);
    MS.Position := 0;
    DTags.LoadFromStream(MS, dfBinary);

    Q := FS.ReadQWord;
    MS.SetSize(Q);
    P := MS.Memory;
    FS.Read(P^, Q);
    MS.Position := 0;
    DPaths.LoadFromStream(MS, dfBinary);

    Q := FS.ReadQWord;
    MS.SetSize(Q);
    P := MS.Memory;
    FS.Read(P^, Q);
    MS.Position := 0;
    DOperations.LoadFromStream(MS, dfBinary);

    Q := FS.ReadQWord;
    MS.SetSize(Q);
    P := MS.Memory;
    FS.Read(P^, Q);
    MS.Position := 0;
    DParams.LoadFromStream(MS, dfBinary);

    Q := FS.ReadQWord;
    MS.SetSize(Q);
    P := MS.Memory;
    FS.Read(P^, Q);
    MS.Position := 0;
    DResponses.LoadFromStream(MS, dfBinary);

    MS.Free;
    FS.Free;
  end;
end;

procedure TFormMain.ActionDefinitionReplaceExecute(Sender: TObject);
begin
  ReplaceDialog1.OnFind := @ReplaceDialog1FindDef;
  ReplaceDialog1.OnReplace := @ReplaceDialog1ReplaceDef;
  ReplaceDialog1.Execute;
end;

procedure TFormMain.DS_DDefinitionsDataChange(Sender: TObject; Field: TField);
begin
  if (Field = nil) and (not DDefinitions.FieldByName('Id').IsNull) then
  begin
    DDefinitionFields.Filter := 'DefinitionId=' + IntToStr(DDefinitions.FieldByName('Id').AsInteger);
  end;
end;

procedure TFormMain.DS_DOperationsDataChange(Sender: TObject; Field: TField);
begin
  if (Field = nil) and (not DOperations.FieldByName('Id').IsNull) then
  begin
    DParams.Filter := 'OperationId=' + IntToStr(DOperations.FieldByName('Id').AsInteger);
    DResponses.Filter := 'OperationId=' + IntToStr(DOperations.FieldByName('Id').AsInteger);
  end;
end;

procedure TFormMain.DS_DPathsDataChange(Sender: TObject; Field: TField);
begin
  if (Field = nil) and (not DPaths.FieldByName('Id').IsNull) then
  begin
    DOperations.Filter := 'PathId=' + IntToStr(DPaths.FieldByName('Id').AsInteger);
  end;
end;

procedure TFormMain.DS_DTagsDataChange(Sender: TObject; Field: TField);
begin
  if (Field = nil) and (not DTags.FieldByName('Id').IsNull) then
  begin
    DPaths.Filter := 'TagId=' + IntToStr(DTags.FieldByName('Id').AsInteger);
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  DDefinitions.CreateDataset;
  DDefinitionFields.CreateDataset;
  DTags.CreateDataset;
  DPaths.CreateDataset;
  DOperations.CreateDataset;
  DParams.CreateDataset;
  DResponses.CreateDataset;

  DDefinitions.Open;
  DDefinitionFields.Open;
  DTags.Open;
  DPaths.Open;
  DOperations.Open;
  DParams.Open;
  DResponses.Open;
end;

procedure TFormMain.ReplaceDialog1FindDef(Sender: TObject);
begin

end;

procedure TFormMain.ReplaceDialog1ReplaceDef(Sender: TObject);
var
  DD, DF: TBookMark;
begin
  DD := DDefinitions.GetBookmark;
  DF := DDefinitionFields.GetBookmark;
  DDefinitionFields.DisableControls;
  DDefinitions.DisableControls;
  DDefinitions.First;
  while not DDefinitions.EOF do
  begin
    if not DDefinitions.FieldByName('PasName').IsNull then
    begin
      DDefinitions.Edit;
      DDefinitions.FieldByName('PasName').AsString := StringReplace(
        DDefinitions.FieldByName('PasName').AsString, ReplaceDialog1.FindText, ReplaceDialog1.ReplaceText, [rfIgnoreCase, rfReplaceAll]);
      DDefinitions.Post;
    end;
    DDefinitions.Next;
  end;
  DDefinitions.GotoBookmark(DD);
  DDefinitionFields.GotoBookmark(DF);
  DDefinitions.FreeBookmark(DD);
  DDefinitionFields.FreeBookmark(DF);
  DDefinitionFields.EnableControls;
  DDefinitions.EnableControls;
end;

function TFormMain.DefinitionLookup(ARef: String; AField: String): Variant;
const
  CREFPTH = '#/definitions/';
begin
  if Copy(ARef, 1, Length(CREFPTH)) = CREFPTH then
    ARef := Copy(ARef, Length(CREFPTH) + 1, Length(ARef));
  Result := DDefinitions.Lookup('Name', ARef, AField);
end;

function TFormMain.DefinitionLookupPasName(ARef: String): String;
var
  V: Variant;
begin
  V := DefinitionLookup(ARef, 'PasName');
  if VarIsStr(V) then
    Result := V
  else
    Result := '';
end;

function TFormMain.DefinitionLookupId(ARef: String): Integer;
var
  V: Variant;
begin
  V := DefinitionLookup(ARef, 'Id');
  if VarIsNumeric(V) then
    Result := V
  else
    Result := -1;
end;

procedure TFormMain.DefinitionLookupIdPasName(ARef: String; out AID: Integer;
  out APasName: String);
var
  V: Variant;
begin
  V := DefinitionLookup(ARef, 'Id;PasName');
  if VarIsArray(V) then
  begin
    if VarIsNumeric(V[0]) then
      AID := V[0]
    else
      AID := -1;
    if VarIsStr(V[1]) then
      APasName := V[1]
    else
      APasName := '';
  end;
end;

end.

