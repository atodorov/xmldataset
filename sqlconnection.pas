unit sqlconnection;

{$mode objfpc}{$H+}

{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!

 Implements TBaseSQLConnection - base interface to handle multiple connections
 and transactions, used by TBaseXMLQuery.

 (c) 2005 by Alexander Todorov.
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

interface

uses
  Classes, SysUtils; 

type

  TBaseSQLConnection = class(TComponent)
  private
    FDatasets : TList; // list of all datasets that use this connection
    FInTransaction : Boolean; // transaction handling
    
  end;
  
implementation

end.

