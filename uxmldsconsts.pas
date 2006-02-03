unit uxmldsconsts;

{$mode objfpc}{$H+}

{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!

 Constants describing the xml structure of the format used by XMLDataset componets

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

// XML attribute name / value constants
  cCount = 'count';
  cYes   = 'yes';
  cNo    = 'no';
  cTrue  = 'true';
  cFalse = 'false';

// row state constants. bit wise or
  ROW_NOT_MODIFIED = $00000000;
  ROW_INSERTED     = $00000001;
  ROW_MODIFIED     = $00000002;

// field kinds
  FK_DATA = 'data';
  FK_CALCULATED = 'calculated';

const
// query type constants
  QUERY_INSERT = 'insert';
  QUERY_DELETE = 'delete';
  QUERY_SELECT = 'select';
  QUERY_UPDATE = 'update';
  QUERY_DO     = 'do'; // anything else : create table, stored procs, etc

// field data type constants
(*
  FIELD_DATATYPE_STRING  = 'string';
  FIELD_DATATYPE_INTEGER = 'integer';
*)

// XML node and attribute name constants


// global xml constants
{+}cDocument = 'document';
{| [DOCUMENT ATTRIBUTES]}
{|}cDocument_Type = 'type';
{|}cDocument_Type_SQL = 'sql';
{|}cDocument_Type_Smart_SQL = 'smart_sql';
{|}cDocument_Type_Datapacket = 'datapacket';
{.}

//<?xml version="1.0" ?>
{+ <document type="datapacket">}
{| [DOCUMENT DATAPACKET ATTRIBUTES] }
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
{|  +--+}cUpdateMode = 'update_mode';
{|  |    [UPDATE_MODE ATTRIBUTES] }
{|  |}   cUpdateMode_Value = 'value';
{|  +--+}cFieldDefs = 'fielddefs';
{|  |  +---}cFieldDef = 'fielddef';
{|  |       [FIELDDEF ATTRIBUTES] }
{|  |}      cFieldDef_Name          = 'name';
{|  |}      cFieldDef_FieldKind     = 'fieldkind';
{|  |}      cFieldDef_DataType      = 'datatype';
{|  |}      cFieldDef_FieldSize     = 'fieldsize';
{|  |}      cFieldDef_DisplayLabel  = 'displaylabel';
{|  |}      cFieldDef_DisplayWidth  = 'displaywidth';
{|  |}      cFieldDef_DisplayFormat = 'displayformat';
{|  |}      cFieldDef_FieldIndex    = 'fieldindex';
{|  |}      cFieldDef_Required      = 'required';
{|  |}      cFieldDef_ReadOnly      = 'readonly';
{|  |}      cFieldDef_Visible       = 'visible';
{|  +---}cIndexDefs = 'indexdefs';
{+--+}cRecordData = 'recorddata';
{|  +--+}cRow = 'row';
{|     | [ROW ATTRIBUTES] }
{|     |}cRow_ID = 'id';       // internal id
{|     |}cRow_State = 'state'; // not modified, inserted, modified
{|     +---}cField = 'field';
{|          [FIELD ATTRIBUTES] }
{|}         cField_Name     = 'name';
{|}         cField_Value    = 'value';
{|}         cField_OldValue = 'oldvalue';
{+--+}cDeletedRecords = 'deletedrecords';
{|  +--- count <row>'s ... }
{.}

// sql xml constants
{+ <document type="sql_xml">}
{|---}cQuery = 'query';
{|    [QUERY ATTRIBUTES] }
{|   }cQuery_Type = 'type';
{.}

// smart sql xml constants
{+ <document type="smart_sql">}
{|}cInsert = 'insert';
{|}cUpdate = 'update';
{|}cDelete = 'delete';
{.}

implementation

end.
