program TestXMLDS;
///
{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, smartxmlquery, tcpsqlconn, xmldataset;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

