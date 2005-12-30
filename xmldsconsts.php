<?php

/*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!

 Constants describing the xml structure of the format used by XMLDataset componets
 This file is intended for use inside php scripts that work with xml
 produced by any of XMLDataset componet.

 (c) Alexander Todorov
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
*******************************************************************************/

// XML attribute name / value constants
  define('C_COUNT', 'count');
  define('C_YES',   'yes');
  define('C_NO',    'no');
  define('C_TRUE',  'true');
  define('C_FALSE', 'false');

// row state constants. bit wise or
  define('ROW_NOT_MODIFIED', 0);
  define('ROW_INSERTED',     1);
  define('ROW_MODIFIED',     2);

// field kinds
  define('FK_DATA', 'data');
  define('FK_CALCULATED', 'calculated');

// update mode constants
  define('UM_WHERE_ALL',      0);
  define('UM_WHERE_CHANGED',  1);
  define('UM_WHERE_KEY_ONLY', 2);

// query type constants
  define('QUERY_INSERT', 'insert');
  define('QUERY_DELETE', 'delete');
  define('QUERY_SELECT', 'select');
  define('QUERY_UPDATE', 'update');
  define('QUERY_DO',     'do');

// XML node and attribute name constants

// global xml constants
/*+*/define('C_DOCUMENT', 'document');
/*| [DOCUMENT ATTRIBUTES]*/
/*|*/define('C_DOCUMENT_TYPE',            'type');
/*|*/define('C_DOCUMENT_TYPE_SQL',        'sql');
/*|*/define('C_DOCUMENT_TYPE_SMART_SQL',  'smart_sql');
/*|*/define('C_DOCUMENT_TYPE_DATAPACKET', 'datapacket');
/*.*/

//<?xml version="1.0" ?>
/*+ <document type="datapacket">*/
/*| [DOCUMENT DATAPACKET ATTRIBUTES] */
/*|*/define('C_DATAPACKET_VERSION',     'version');
/*|*/define('C_DATAPACKET_DESTINATION', 'destination');
/*|*/define('C_DATAPACKET_DAY',         'day');
/*|*/define('C_DATAPACKET_MONTH',       'month');
/*|*/define('C_DATAPACKET_YEAR',        'year');
/*|*/define('C_DATAPACKET_HOUR',        'hour');
/*|*/define('C_DATAPACKET_MIN',         'min');
/*|*/define('C_DATAPACKET_SEC',         'sec');
/*|*/define('C_DATAPACKET_MSEC',        'msec');
/*+---*/define('C_PRODUCER', 'producer');
/*|    [PRODUCER ATTRIBUTES] */
/*|*/   define('C_PRODUCER_NAME',        'name');
/*|*/   define('C_PRODUCER_URL',         'url');
/*|*/   define('C_PRODUCER_DESCRIPTION', 'description');
/*+--+*/define('C_METADATA',             'metadata');
/*|  | [METADATA ATTRIBUTES] */
/*|  |*/define('C_METADATA_FIELDDEFS',  'fielddefs');
/*|  |*/define('C_METADATA_INDEXDEFS',  'indexdefs');
/*|  |*/define('C_METADATA_RECORDDATA', 'recorddata');
/*|  |*/define('C_METADATA_CHANGES',    'changes');
/*|  +---*/define('C_TABLE', 'table');
/*|  |    [TABLE ATTRIBUTES] */
/*|  |*/   define('C_TABLE_NAME',  'name');
/*|  +--+*/define('C_CONSTRAINTS', 'constraints');
/*|  |  +--+*/define('C_KEYS', 'keys');
/*|  |  |  +---*/define('C_PRIMARYKEY', 'primary_key');
/*|  |  |  |    [PRIMARY KEY ATTRIBUTES] */
/*|  |  |  |*/   define('C_PRIMARYKEY_NAME',  'name');
/*|  |  |  |*/   define('C_PRIMARYKEY_FIELD', 'field');
/*|  |  |  +---*/define('C_FOREIGNKEY',       'foreign_key');
/*|  |  |       [FOREIGN KEY ATTRIBUTES] */
/*|  |  |*/      define('C_FOREIGNKEY_NAME',     'name');
/*|  |  |*/      define('C_FOREIGNKEY_FIELD',    'field');
/*|  |  |*/      define('C_FOREIGNKEY_REFTABLE', 'reftable');
/*|  |  |*/      define('C_FOREIGNKEY_REFFIELD', 'reffield');
/*|  |  |*/      define('C_FOREIGNKEY_ACTION',   'action');
/*|  |  +---*/define('C_UNIQUES', 'uniques');
/*|  |  +---*/define('C_CHECKS',  'checks');
/*|  +--+*/define('C_UPDATEMODE', 'update_mode');
/*|  |    [UPDATE_MODE ATTRIBUTES] */
/*|  |*/   define('C_UPDATEMODE_VALUE', 'value');
/*|  +--+*/define('C_FIELDDEFS', 'fielddefs');
/*|  |  +---*/define('C_FIELDDEF', 'fielddef');
/*|  |       [FIELDDEF ATTRIBUTES] */
/*|  |*/      define('C_FIELDDEF_NAME',          'name');
/*|  |*/      define('C_FIELDDEF_FIELDKIND',     'fieldkind');
/*|  |*/      define('C_FIELDDEF_DATATYPE',      'datatype');
/*|  |*/      define('C_FIELDDEF_FIELDSIZE',     'fieldsize');
/*|  |*/      define('C_FIELDDEF_DISPLAYLABEL',  'displaylabel');
/*|  |*/      define('C_FIELDDEF_DISPLAYWIDTH',  'displaywidth');
/*|  |*/      define('C_FIELDDEF_DISPLAYFORMAT', 'displayformat');
/*|  |*/      define('C_FIELDDEF_FIELDINDEX',    'fieldindex');
/*|  |*/      define('C_FIELDDEF_REQUIRED',      'required');
/*|  |*/      define('C_FIELDDEF_READONLY',      'readonly');
/*|  |*/      define('C_FIELDDEF_VISIBLE',       'visible');
/*|  +---*/define('C_INDEXDEFS', 'indexdefs');
/*+--+*/define('C_RECORDDATA', 'recorddata');
/*|  +--+*/define('C_ROW', 'row');
/*|     | [ROW ATTRIBUTES] */
/*|     |*/define('C_ROW_ID',    'id');
/*|     |*/define('C_ROW_STATE', 'state');
/*|     +---*/define('C_FIELD', 'field');
/*|          [FIELD ATTRIBUTES] */
/*|*/         define('C_FIELD_NAME',     'name');
/*|*/         define('C_FIELD_VALUE',    'value');
/*|*/         define('C_FIELD_DATATYPE', 'datatype');
/*|*/         define('C_FIELD_OLDVALUE', 'oldvalue');
/*+--+*/define('C_DELETEDRECORDS', 'deletedrecords');
/*|  +--- count <row>'s ... */
/*.*/

// sql xml constants
/*+ <document type="sql_xml">*/
/*|---*/define('C_QUERY', 'query');
/*|    [QUERY ATTRIBUTES] */
/*|   */define('C_QUERY_TYPE', 'type');
/*.*/

// smart sql xml constants
/*+ <document type="smart_sql">*/
/*|*/define('C_INSERT', 'insert');
/*|*/define('C_UPDATE', 'update');
/*|*/define('C_DELETE', 'delete');
/*.*/

?>