unit basexmldataset;

{$mode objfpc}{$H+}

{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Pacal Components Library

 (c) 2005 - Alexander Todorov.
 e-mail: alexx.todorov@gmail.com
 
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING included in this distribution,                      *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
*******************************************************************************}

// a work arround widestrings. We use this because DOMString = WideString
{$DEFINE USEWIDESTRINGS}

{$DEFINE DEBUGXML}

interface

uses
  Classes, SysUtils, DB, DOM, gxBaseDataset
  {$IFDEF DEBUGXML}
    ,StdCtrls;
    var Memo : TMemo;
  {$ELSE}
  ;
  {$ENDIF}

type
  { TBaseXMLDataSet }

  // a base class of dataset working with xml.
  TBaseXMLDataSet=class(TGXBaseDataset)
  private
    FCurRec: Integer;
    FReadOnly: Boolean;
    FXMLDoc : TXMLDocument;
    FNode : TDOMElement;
    procedure SetReadOnly(Value: Boolean);
    procedure SetXMLDoc(const AValue: TXMLDocument);
  protected {Simplified Dataset methods}
    function  DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoDeleteRecord; override;
    procedure DoCreateFieldDefs; override;
    function  GetFieldValue(Field: TField): Variant; override;
    procedure SetFieldValue(Field: TField; Value: Variant); override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    procedure SetBlobField(Field: TField; Stream: TStream); override;
    procedure DoFirst; override;
    procedure DoLast; override;
    function  Navigate(GetMode: TGetMode): TGetResult; override;
    //Record ID functions
    function  AllocateRecordID: Pointer; override;
    procedure DisposeRecordID(Value: Pointer); override;
    procedure GotoRecordID(Value: Pointer); override;
    //Bookmark functions
    function  GetBookMarkSize: Integer; override;
    procedure AllocateBookMark(RecordID: Pointer; ABookmark: Pointer); override;
    procedure DoGotoBookmark(ABookmark: Pointer); override;
    //Others
    procedure DoBeforeGetFieldValue; override;
    procedure DoAfterGetFieldValue; override;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); override;
    procedure DoAfterSetFieldValue(Inserting: Boolean); override;
    {Overriden datatset methods}
    function  GetCanModify: Boolean; override;
    function  GetRecordCount: Integer; override;
    function  GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; AXMLDoc : TXMLDocument); virtual; overload;
    constructor Create(AOwner: TComponent; AXML : String); virtual; overload;
    destructor Destroy; override;
//    property OutlookFolder: TOutlookFolder read FOutlookFolder write SetOutlookFolder;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property XMLDocument : TXMLDocument read FXMLDoc write SetXMLDoc;
  end;

  
  { helper functions }
  function GetFieldTypeFromString(FieldType : String) : TFieldType;
  function GetFieldSizeByType(const FieldType : TFieldType; const Size : Integer = 0) : Integer;
  function GetFieldSizeFromXML(FieldNode : TDOMElement; const Size : Integer = 0) : Integer;
  function GetFieldNodeByName(const AParent : TDOMElement; AFieldName : String) : TDOMElement;
  
implementation

uses uXMLDSConsts;

{$IFDEF DEBUGXML}
procedure Log(const Msg : String);
begin
  Memo.Lines.Add(DateTimeToStr(Now)+' : '+Msg);
end;
{$ENDIF}

(*******************************************************************************
{ helper functions }
*******************************************************************************)

function GetFieldTypeFromString(FieldType : String) : TFieldType;
begin
   FieldType := AnsiUpperCase(FieldType);
   Result := ftUnknown;
        if (FieldType = 'UNKNOWN')     then Result := ftUnknown
   else if (FieldType = 'STRING')      then Result := ftString
   else if (FieldType = 'SMALLINT')    then Result := ftSmallint
   else if (FieldType = 'INTEGER')     then Result := ftInteger
   else if (FieldType = 'WORD')        then Result := ftWord
   else if (FieldType = 'BOOLEAN')     then Result := ftBoolean
   else if (FieldType = 'FLOAT')       then Result := ftFloat
   else if (FieldType = 'CURRENCY')    then Result := ftCurrency
   else if (FieldType = 'BCD')         then Result := ftBCD
   else if (FieldType = 'DATE')        then Result := ftDate
   else if (FieldType = 'TIME')        then Result := ftTime
   else if (FieldType = 'DATETIME')    then Result := ftDateTime
   else if (FieldType = 'BYTES')       then Result := ftBytes
   else if (FieldType = 'VARBYTES')    then Result := ftVarBytes
   else if (FieldType = 'AUTOINC')     then Result := ftAutoInc
   else if (FieldType = 'BLOB')        then Result := ftBlob
   else if (FieldType = 'MEMO')        then Result := ftMemo
   else if (FieldType = 'GRAPHIC')     then Result := ftGraphic
   else if (FieldType = 'FMTMEMO')     then Result := ftFmtMemo
   else if (FieldType = 'PARADOXOLE')  then Result := ftParadoxOle
   else if (FieldType = 'DBASEOLE')    then Result := ftDBaseOle
   else if (FieldType = 'TYPEDBINARY') then Result := ftTypedBinary
   else if (FieldType = 'CURSOR')      then Result := ftCursor
   else if (FieldType = 'FIXEDCHAR')   then Result := ftFixedChar
   else if (FieldType = 'WIDESTRING')  then Result := ftWideString
   else if (FieldType = 'LARGEINT')    then Result := ftLargeint
   else if (FieldType = 'ADT')         then Result := ftADT
   else if (FieldType = 'ARRAY')       then Result := ftArray
   else if (FieldType = 'REFERENCE')   then Result := ftReference
   else if (FieldType = 'DATASET')     then Result := ftDataSet
   else if (FieldType = 'ORABLOB')     then Result := ftOraBlob
   else if (FieldType = 'ORACLOB')     then Result := ftOraClob
   else if (FieldType = 'VARIANT')     then Result := ftVariant
   else if (FieldType = 'INTERFACE')   then Result := ftInterface
   else if (FieldType = 'IDISPATCH')   then Result := ftIDispatch
   else if (FieldType = 'GUID')        then Result := ftGuid
   else if (FieldType = 'TIMESTAMP')   then Result := ftTimeStamp
   else if (FieldType = 'FMTBCD')      then Result := ftFMTBcd;
end;

function GetFieldSizeByType(const FieldType : TFieldType; const Size : Integer = 0) : Integer;
begin
    Result := Size;
    case FieldType of
     ftUnknown           : ;
     ftString            : ;
     ftSmallint          : Result := SizeOf(Byte); //todo: check this
     ftInteger           : Result := SizeOf(Integer);
     ftWord              : Result := SizeOf(Word);
     ftBoolean           : Result := SizeOf(Boolean);
     ftFloat             : Result := SizeOf(Double);
     ftCurrency          : Result := SizeOf(Currency);
     ftBCD               : ;
     ftDate, ftDateTime, ftTime, ftTimeStamp  : Result := SizeOf(TDateTime);
     ftBytes             : ;
     ftVarBytes          : ;
     ftAutoInc           : ;
     ftBlob              : ;
     ftMemo              : ;
     ftGraphic           : ;
     ftFmtMemo           : ;
     ftParadoxOle        : ;
     ftDBaseOle          : ;
     ftTypedBinary       : ;
     ftCursor            : ;
     ftFixedChar         : ;
     ftWideString        : ;
     ftLargeint          : ;
     ftADT               : ;
     ftArray             : ;
     ftReference         : ;
     ftDataSet           : ;
     ftOraBlob           : ;
     ftOraClob           : ;
     ftVariant           : ;
     ftInterface         : ;
     ftIDispatch         : ;
     ftGuid              : ;
     ftFMTBcd            : ;
   end;
end;

function GetFieldSizeFromXML(FieldNode : TDOMElement; const Size : Integer = 0) : Integer;
begin
   Result := Size;
   if (FieldNode <> nil) and (FieldNode.AttribStrings[cField_Size] <> '') then
      Result := StrToInt(FieldNode.AttribStrings[cField_Size]);
end;

function GetFieldNodeByName(const AParent : TDOMElement; AFieldName : String) : TDOMElement;
var i : Integer;
begin
// AParent = <row> ... </row>
// ChildNodes.Item[i] = <field ... />
  Result := nil;
  AFieldName := AnsiUpperCase(AFieldName);
  {$IFDEF DEBUGXML}
  {$ENDIF}
    
  for i := 0 to AParent.ChildNodes.Count - 1 do
    begin
    {$IFDEF DEBUGXML}
    {$ENDIF}
      if (AnsiUpperCase(TDOMElement(AParent.ChildNodes.Item[i]).AttribStrings[cField_Name]) = AFieldName) then
        begin
          Result := TDOMElement(AParent.ChildNodes.Item[i]);
          exit;
        end;
    end;
end;

(*******************************************************************************
{ TBaseXMLDataSet }
*******************************************************************************)

procedure TBaseXMLDataSet.SetReadOnly(Value: Boolean);
begin
  if (Value <> FReadOnly) then
    begin
     if Active then DatabaseError('Cannot change readonly property when dataset is active');
     FReadOnly:=Value;
    end;
end;

procedure TBaseXMLDataSet.SetXMLDoc(const AValue: TXMLDocument);
begin
  if Active then
     DatabaseError('Cannot change XMLDoc property when dataset is active');
  FXMLDoc := AValue;
end;

function TBaseXMLDataSet.DoOpen: Boolean;
begin
  if not Assigned(FXMLDoc) then
     DatabaseError('XMLDoc is not assigned');
  FCurRec:=-1;
  Result := true; //todo : true or <rowdata>.childs.count > 0 ?
end;

procedure TBaseXMLDataSet.DoClose;
begin
  FNode := nil;
//  FXMLDoc := nil; //todo : check this
end;

procedure TBaseXMLDataSet.DoDeleteRecord;
begin
// mark as deleted
  FXMLDoc.DocumentElement.FindNode(cDeletedRecords).AppendChild(FNode.CloneNode(true,FXMLDoc));
// remove
  FXMLDoc.DocumentElement.FindNode(cRecordData).RemoveChild
      (FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec]);

  if FCurRec >= FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count then
     Dec(FCurRec);
end;

procedure TBaseXMLDataSet.DoCreateFieldDefs;
var i, FieldSize :Integer;
    domNode : TDOMNode;
    FieldName : String;
    Required : Boolean;
    ftFieldType : TFieldType;
begin
  if (not Assigned(FXMLDoc)) or
     (csDesigning in ComponentState) then
     exit;

  FieldDefs.Clear;
  for i := 0 to FXMLDoc.DocumentElement.FindNode(cMetaData).FindNode(cFieldDefs).ChildNodes.Count - 1 do
      begin   // Add fields
        domNode := FXMLDoc.DocumentElement.FindNode(cMetadata).FindNode(cFieldDefs).ChildNodes.Item[i];
        FieldName := Trim(domNode.Attributes.GetNamedItem(cFieldDef_Name).NodeValue);
        ftFieldType := GetFieldTypeFromString(Trim(domNode.Attributes.GetNamedItem(cFieldDef_DataType).NodeValue));
        Required := AnsiLowerCase(domNode.Attributes.GetNamedItem(cFieldDef_Required).NodeValue) = cTrue;
        // determine field size
        FieldSize := GetFieldSizeByType(ftFieldType,StrToInt(domNode.Attributes.GetNamedItem(cFieldDef_FieldSize).NodeValue));

        FieldDefs.Add(FieldName, ftFieldType, FieldSize, Required);
        FieldDefs.Items[i].DisplayName := domNode.Attributes.GetNamedItem(cFieldDef_DisplayLabel).NodeValue;
      end;
end;

function TBaseXMLDataSet.GetFieldValue(Field: TField): Variant;
var FieldNode : TDOMElement;
begin
   FieldNode := GetFieldNodeByName(FNode,Field.FieldName);
{$IFDEF USEWIDESTRINGS}
   Result := WideCharToString(@FieldNode.AttribStrings[cField_Value][1]);
{$ELSE}
   Result := FieldNode.AttribStrings[cField_Value];
{$ENDIF}
end;

procedure TBaseXMLDataSet.SetFieldValue(Field: TField; Value: Variant);
var FieldNode : TDOMElement;
begin
   FieldNode := GetFieldNodeByName(FNode,Field.FieldName);
// here it should be no problem assigning String to WideString;
   FieldNode.AttribStrings[cField_Value] := Value;
end;

procedure TBaseXMLDataSet.GetBlobField(Field: TField; Stream: TStream);
begin
//todo : implement
end;

procedure TBaseXMLDataSet.SetBlobField(Field: TField; Stream: TStream);
begin
//todo : implement
end;

procedure TBaseXMLDataSet.DoFirst;
begin
  FCurRec := -1;
end;

procedure TBaseXMLDataSet.DoLast;
begin
  FCurRec := RecordCount;
end;

function TBaseXMLDataSet.Navigate(GetMode: TGetMode): TGetResult;
begin
  if (RecordCount < 1)
    then Result := grEOF
    else
      begin
        Result:=grOK;
        case GetMode of
          gmNext:    if (FCurRec >= RecordCount-1)
                       then Result := grEOF
                       else Inc(FCurRec);
          gmPrior:   if (FCurRec <= 0)
                       then begin Result := grBOF; FCurRec := -1; end
                       else Dec(FCurRec);
          gmCurrent: if (FCurRec < 0) or (FCurRec >= RecordCount) then
                        Result := grError;
        end; // case
      end; // else
end;

function TBaseXMLDataSet.AllocateRecordID: Pointer;
begin
  Result := Pointer(FCurRec);
end;

procedure TBaseXMLDataSet.DisposeRecordID(Value: Pointer);
begin
  //Do nothing, no need to dispose since pointer is just an integer
end;

procedure TBaseXMLDataSet.GotoRecordID(Value: Pointer);
begin
  FCurRec := Integer(Value);
end;

function TBaseXMLDataSet.GetBookMarkSize: Integer;
begin
  Result := SizeOf(Integer);
end;

procedure TBaseXMLDataSet.AllocateBookMark(RecordID: Pointer; ABookmark: Pointer);
begin
  PInteger(ABookmark)^:=Integer(RecordID);
end;

procedure TBaseXMLDataSet.DoGotoBookmark(ABookmark: Pointer);
begin
  GotoRecordID(Pointer(PInteger(ABookmark)^));
end;

procedure TBaseXMLDataSet.DoBeforeGetFieldValue;
begin
  FNode := TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec]);
end;

procedure TBaseXMLDataSet.DoAfterGetFieldValue;
begin
  if not ((FNode <> nil) and (State = dsInsert)) then
     FNode := nil;
end;

procedure TBaseXMLDataSet.DoBeforeSetFieldValue(Inserting: Boolean);
var NewChild :TDOMElement;
begin
  try
    NewChild := TDOMElement.Create(FXMLDoc);
    if Inserting
      then FNode := TDOMElement(TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData)).AppendChild(NewChild))
      else FNode := TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec]);
  finally
    NewChild.Free;   // todo: clear or not ??
  end;
end;

procedure TBaseXMLDataSet.DoAfterSetFieldValue(Inserting: Boolean);
begin
//todo : check this out
end;

function TBaseXMLDataSet.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TBaseXMLDataSet.GetRecordCount: Integer;
begin
  Result := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count;
end;

function TBaseXMLDataSet.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0)
    then Result := 1
    else Result := FCurRec + 1;
end;

procedure TBaseXMLDataSet.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value < RecordCount) then
    begin
      FCurRec := Value-1;
      Resync([]);
    end;
end;

constructor TBaseXMLDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLDoc := TXMLDocument.Create;
end;

constructor TBaseXMLDataSet.Create(AOwner: TComponent; AXMLDoc: TXMLDocument);
begin
  Self.Create(AOwner);
  FXMLDoc := AXMLDoc;
end;

constructor TBaseXMLDataSet.Create(AOwner: TComponent; AXML: String);
begin
  Self.Create(AOwner);
  FXMLDoc.DocumentElement.NodeValue := AXML;
end;

destructor TBaseXMLDataSet.Destroy;
begin
  FXMLDoc.Free;
  inherited Destroy;
end;

end.

