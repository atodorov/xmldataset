unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  DB, Buttons, StdCtrls, DBCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DBNavigator1: TDBNavigator;
    dsMain: TDatasource;
    dbGrid1: TdbGrid;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

uses basexmldataset, XMLRead, XMLWrite;


var XMLDS : TBaseXMLDataSet;
    
{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ReadXMLFile(XMLDS.XMLDocument,'D:/projects/TXMLFormatDataSet/data.xml');
  XMLDS.ReadOnly := false;// true;
  XMLDS.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  WriteXMLFile(XMLDS.XMLDocument,'D:/projects/TXMLFormatDataSet/save.xml');
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
//{$IFDEF DEBUGXML}
  baseXMLDataSet.Memo := Memo1;
//{$ENDIF}
  XMLDS := TBaseXMLDataSet.Create(Self);
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

