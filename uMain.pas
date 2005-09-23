unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  DB, Buttons, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    dsMain: TDatasource;
    dbGrid1: TdbGrid;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
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

uses XMLFormatDataSet, XMLRead, gxBaseDataSet;


var gxXML : TgxXMLDataSet;
    
{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ReadXMLFile(gxXML.XMLDocument,'D:\projects\TXMLFormatDataSet\data.xml');
  gxXML.ReadOnly := true;
  gxXML.Open;
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
//{$IFDEF DEBUGXML}
  XMLFormatDataSet.Memo := Memo1;
  gxBaseDataSet.memoLog := Memo1;
//{$ENDIF}
  gxXML := TgxXMLDataSet.Create(Self);
  dsMain.DataSet := gxXML;
end;

procedure TForm1.Form1Destroy(Sender: TObject);
begin
  if gxXML.Active then
     gxXML.Close;
  gxXML.Free;
end;

initialization
  {$I uMain.lrs}

end.

