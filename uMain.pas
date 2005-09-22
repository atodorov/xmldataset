unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, DB;

type

  { TfMain }

  TfMain = class(TForm)
   btnLoad: TButton;
   btnSave: TButton;
   dsMain: TDatasource;
   dbgMain: TdbGrid;
   procedure FormCreate(Sender: TObject);
   procedure FormDestroy(Sender: TObject);
   procedure btnLoadClick(Sender: TObject);
   procedure btnSaveClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fMain: TfMain;

implementation

uses XMLFormatDataSet, XMLRead;


var XMLDS : TXMLFormatDataSet;
{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin
  XMLDS := TXMLFormatDataSet.Create(Self);
  dsMain.DataSet := XMLDS;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  if XMLDS.Active then
     XMLDS.Close;
  XMLDS.Free;
end;

procedure TfMain.btnLoadClick(Sender: TObject);
begin
  ReadXMLFile(XMLDS.XMLDocument,'data.xml');
  XMLDS.ReadOnly := true;
  XMLDS.Open;
end;

procedure TfMain.btnSaveClick(Sender: TObject);
begin
// XMLDS.SaveToFile('save.xml');
end;

initialization
  {$I umain.lrs}

end.

