unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  DB, Buttons, StdCtrls, DBCtrls, ExtDlgs, ExtCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    DateEdit1: TDateEdit;
    DBNavigator1: TDBNavigator;
    dsMain: TDatasource;
    dbGrid1: TdbGrid;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure DBImage1Click(Sender: TObject);
    procedure DateEdit1Exit(Sender: TObject);
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses uXMLDSConsts, CustomXMLDataset, HTTPSQLConn, XMLQuery, SmartXMLQuery,
//     DBCtrlGrid in 'dbcg/dbctrlgrid.pas',
     XMLRead, XMLWrite;


var XMLDS : TCustomXMLDataSet;
    
{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  XMLDS.Close;
  ReadXMLFile(XMLDS.XMLDocument,ExtractFilePath(Application.ExeName)+PathDelim+'data.xml');
  XMLDS.ReadOnly := false;// true;
  XMLDS.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  WriteXMLFile(XMLDS.XMLDocument,ExtractFilePath(Application.ExeName)+PathDelim+'save.xml');
end;

procedure TForm1.Button3Click(Sender: TObject);
var Conn : THttpSQLConnection;
    ssResult : TStringStream;
begin
  try
    Memo1.Lines.Clear;
    Conn := THTTPSQLConnection.Create(Self);
    Conn.ConnParams.Add(HTTP_URL+'=http://alexx.itafree.com/cgi-bin/showupload.pl');
    Conn.ConnParams.Add(HTTP_POST_FILENAME+'=file.xml');
    Conn.ConnParams.Add(HTTP_POST_FIELDNAME+'=xml_file');
    Conn.DataToSend := XMLDS.XMLStringStream;
    Conn.ReceivedData := ssResult;
    Conn.Open;
    Memo1.Lines.LoadFromStream(ssResult);
  finally
    if Assigned(Conn) then
       Conn.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var XQ : TCustomXMLQuery;
begin
   xq := TCustomXMLQuery.Create(Self);
   xq.SQL.Text := 'SELECT * FROM COUNTRY';
   xq.ExecSQL(QUERY_SELECT);
   
   xq.SQL.Text := 'SELECT * FROM CITY';
   xq.ExecSQL(QUERY_SELECT);
   
   xq.Free;
end;

procedure TForm1.Button5Click(Sender: TObject);
var sq : TSmartXMLQuery;
begin
   sq := TSmartXMLQuery.Create(Self);
   sq.XMLDocument := XMLDS.XMLDocument;
   sq.SQLConnBeforeCommitDataset(nil,sq);
   sq.Free;
end;

procedure TForm1.DBImage1Click(Sender: TObject);
begin
  with TOpenPictureDialog.Create(Self) do
   try
     if Execute then
       begin
//         DBImage1.Picture.LoadFromFile(FileName);
//         TGraphicField(XMLDS.FieldByName('FLAG')).LoadFromFile(FileName);
       end;
   finally
     Free;
   end;
end;

procedure TForm1.DateEdit1Exit(Sender: TObject);
begin
  XMLDS.FieldByName('Дата').AsDateTime := DateEdit1.Date;
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
//{$IFDEF DEBUGXML}
  CustomXMLDataSet.Memo := Memo1;
//{$ENDIF}
  XMLDS := TCustomXMLDataSet.Create(Self);
  XMLDS.UseBase64 := false;
  dsMain.DataSet := XMLDS;
end;

procedure TForm1.Form1Destroy(Sender: TObject);
begin
  if XMLDS.Active then
     XMLDS.Close;
  XMLDS.Free;
end;

initialization
  {$I uMain.lrs}

end.

