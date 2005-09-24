unit uxmldsconsts;

{$mode objfpc}{$H+}

{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!

 Constants describing the xml structure of the format used by TBaseXMLDataset.
 
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

const

// XML attribute value constants
  cYes   = 'yes';
  cNo    = 'no';
  cTrue  = 'true';
  cFalse = 'false';

// XML node and attribute name constants

//<?xml version="1.0" ?>
{+}cDatapacket = 'datapacket';
{| [DATAPACKET ATTRIBUTES] }
{|}cDatapacket_Version     = 'version';
{|}cDatapacket_Destination = 'destination';
{|}cDatapacket_Day         = 'day';
{|}cDatapacket_Month       = 'month';
{|}cDatapacket_Year        = 'year';
{|}cDatapacket_Hour        = 'hour';
{|}cDatapacket_Min         = 'min';
{|}cDatapacket_Sec         = 'sec';
{|}cDatapacket_MSec        = 'msec';
{+---}cProducer = 'producer';
{|    [PRODUCER ATTRIBUTES] }
{|}   cProducer_Name        = 'name';
{|}   cProducer_URL         = 'url';
{|}   cProducer_Description = 'description';
{+--+}cMetadata = 'metadata';
{|  | [METADATA ATTRIBUTES] }
{|  |}cMetadata_FieldDefs  = 'fielddefs';
{|  |}cMetadata_IndexDefs  = 'indexdefs';
{|  |}cMetadata_RecordData = 'recorddata';
{|  |}cMetadata_Changes    = 'changes';
{|  +---}cTable = 'table';
{|  |    [TABLE ATTRIBUTES] }
{|  |}   cTable_Name = 'name';
{|  +--+}cConstraints = 'constraints';
{|  |  +--+}cKeys = 'keys';
{|  |  |  +---}cPrimaryKey = 'primary_key';
{|  |  |  |    [PRIMARY KEY ATTRIBUTES] }
{|  |  |  |}   cPrimaryKey_Name  = 'name';
{|  |  |  |}   cPrimaryKey_Field = 'field';
{|  |  |  +---}cForeignKey = 'foreign_key';
{|  |  |       [FOREIGN KEY ATTRIBUTES] }
{|  |  |}      cForeignKey_Name     = 'name';
{|  |  |}      cForeignKey_Field    = 'field';
{|  |  |}      cForeignKey_RefTable = 'reftable';
{|  |  |}      cForeignKey_RefField = 'reffield';
{|  |  |}      cForeignKey_Action   = 'action';
{|  |  +---}cUniques = 'uniques';
{|  |  +---}cChecks  = 'checks';
{|  +--+}cFieldDefs = 'fielddefs';
{|  |  | [FIELDDEFS ATTRIBUTES] }
{|  |  |}cFieldDefs_Count = 'count';
{|  |  +---}cFieldDef = 'fielddef';
{|  |       [FIELDDEF ATTRIBUTES] }
{|  |}      cFieldDef_Name          = 'name';
{|  |}      cFieldDef_FieldKind     = 'fieldkind';
{|  |}      cFieldDef_DataType      = 'datatype';
{|  |}      cFieldDef_FieldSize     = 'fieldsize';
{|  |}      cFieldDef_DisplayLabel  = 'displaylabel';
{|  |}      cFieldDef_DisplayWidth  = 'displaywidth';
{|  |}      cFieldDef_DisplayFormat = 'displayformat'; //float and date fields
{|  |}      cFieldDef_FieldIndex    = 'fieldindex';
{|  |}      cFieldDef_Required      = 'required';
{|  |}      cFieldDef_ReadOnly      = 'readonly';
{|  +---}cIndexDefs = 'indexdefs';
{|       [FIELDDEFS ATTRIBUTES] }
{|}      cIndexDefs_Count = 'count';
{+--+}cRecordData = 'recorddata';
{|  | [RECORDDATA ATTRIBUTES] }
{|  |}cRecordData_Count = 'count';
{|  +--+}cRow = 'row';
{|     +---}cField = 'field';
{|          [FIELD ATTRIBUTES] }
{|}         cField_Name     = 'name';
{|}         cField_Value    = 'value';
{|}         cField_DataType = 'datatype';
{|}         cField_Size     = 'size';
{|}         cField_OldValue = 'oldvalue'; // used in <modifiedrecords>
{|}         cField_NewValue = 'newvalue'; // used in <modifiedrecords>
{+--+}cModifiedRecords = 'modifiedrecords';
{|  | [MODIFIEDRECORDS ATTRIBUTES] }
{|  |}cModifiedRecords_Count = 'count';
{|  +--- count <row>'s ... }
{+--+}cInsertedRecords = 'insertedrecords';
{|  | [INSERTEDRECORDS ATTRIBUTES] }
{|  |}cInsertedRecords_Count = 'count';
{|  +--- count <row>'s ... }
{+--+}cDeletedRecords = 'deletedrecords';
{|  | [DELETEDRECORDS ATTRIBUTES] }
{|  |}cDeletedRecords_Count = 'count';
{|  +--- count <row>'s ... }
{.}
     
implementation

end.
