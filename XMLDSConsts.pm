package XMLDSConsts;

################################################################################
#
#  This file is part of the XMLDataset suite for the Free Component Library!
#
#  Constants describing the xml structure of the format used by XMLDataset componets
#  This file is intended for use inside perl scripts that work with xml
#  produced by any of the XMLDataset components.
#
#  file location : https://svn.openfmi.net/xmldataset/XMLDSConsts.pm
#
#  (c) Alexander Todorov
#  e-mail: alexx.todorov@gmail.com
# 
#  *****************************************************************************
#  *                                                                           *
#  *  See the file COPYING included in this distribution,                      *
#  *  for details about the copyright.                                         *
#  *                                                                           *
#  *  This program is distributed in the hope that it will be useful,          *
#  *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
#  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
#  *                                                                           *
#  *****************************************************************************
################################################################################


# XML attribute name / value constants
sub C_COUNT () { return "count"; }
sub C_YES   () { return "yes";   }
sub C_NO    () { return "no";    }
sub C_TRUE  () { return "true";  }
sub C_FALSE () { return "false"; }

# row state constants. bit wise or
sub ROW_NOT_MODIFIED () { return  0; }
sub ROW_INSERTED     () { return  1; }
sub ROW_MODIFIED     () { return  2; }

# field kinds
sub FK_DATA () { return "data"; }
sub FK_CALCULATED () { return "calculated"; }

# query type constants
sub QUERY_INSERT () { return "insert"; }
sub QUERY_DELETE () { return "delete"; }
sub QUERY_SELECT () { return "select"; }
sub QUERY_UPDATE () { return "update"; }
sub QUERY_DO     () { return "do"; }

# XML node and attribute name constants

# global xml constants
sub C_DOCUMENT () { return "document"; }
#     [DOCUMENT ATTRIBUTES]
      sub C_DOCUMENT_TYPE     () { return "type"; }
      sub C_DOCUMENT_TYPE_SQL () { return "sql"; }
      sub C_DOCUMENT_TYPE_SMART_SQL  () { return "smart_sql"; }
      sub C_DOCUMENT_TYPE_DATAPACKET () { return "datapacket"; }
#

#<?xml version="1.0" ?>
# <document type="datapacket">
# [DOCUMENT DATAPACKET ATTRIBUTES] 
  sub C_DATAPACKET_VERSION     () { return "version"; }
  sub C_DATAPACKET_DESTINATION () { return "destination"; }
  sub C_DATAPACKET_DAY   () { return "day"; }
  sub C_DATAPACKET_MONTH () { return "month"; }
  sub C_DATAPACKET_YEAR  () { return "year"; }
  sub C_DATAPACKET_HOUR  () { return "hour"; }
  sub C_DATAPACKET_MIN   () { return "min"; }
  sub C_DATAPACKET_SEC   () { return "sec"; }
  sub C_DATAPACKET_MSEC  () { return "msec"; }
     sub C_PRODUCER () { return "producer"; }
#          [PRODUCER ATTRIBUTES] 
           sub C_PRODUCER_NAME () { return "name"; }
           sub C_PRODUCER_URL  () { return "url"; }
           sub C_PRODUCER_DESCRIPTION () { return "description"; }
     sub C_METADATA () { return "metadata"; }
#          [METADATA ATTRIBUTES] 
           sub C_METADATA_FIELDDEFS  () { return "fielddefs"; }
           sub C_METADATA_INDEXDEFS  () { return "indexdefs"; }
           sub C_METADATA_RECORDDATA () { return "recorddata"; }
           sub C_METADATA_CHANGES    () { return "changes"; }
        sub C_UPDATEMODE () { return "update_mode"; }
#             [UPDATE_MODE ATTRIBUTES] 
              sub C_UPDATEMODE_VALUE () { return "value"; }
        sub C_FIELDDEFS () { return "fielddefs"; }
           sub C_FIELDDEF () { return "fielddef"; }
#                [FIELDDEF ATTRIBUTES] 
                 sub C_FIELDDEF_NAME () { return "name"; }
                 sub C_FIELDDEF_FIELDKIND () { return "fieldkind"; }
                 sub C_FIELDDEF_DATATYPE () { return "datatype"; }
                 sub C_FIELDDEF_FIELDSIZE () { return "fieldsize"; }
                 sub C_FIELDDEF_PROVIDERFLAGS () { return "providerflags"; }
                 sub C_FIELDDEF_DISPLAYLABEL () { return "displaylabel"; }
                 sub C_FIELDDEF_DISPLAYWIDTH () { return "displaywidth"; }
                 sub C_FIELDDEF_DISPLAYFORMAT () { return "displayformat"; }
                 sub C_FIELDDEF_FIELDINDEX () { return "fieldindex"; }
                 sub C_FIELDDEF_REQUIRED () { return "required"; }
                 sub C_FIELDDEF_READONLY () { return "readonly"; }
                 sub C_FIELDDEF_VISIBLE () { return "visible"; }
        sub C_INDEXDEFS () { return "indexdefs"; }
     sub C_RECORDDATA () { return "recorddata"; }
        sub C_ROW () { return "row"; }
#             [ROW ATTRIBUTES] 
              sub C_ROW_ID () { return "id"; }
              sub C_ROW_STATE () { return "state"; }
           sub C_FIELD () { return "field"; }
#                [FIELD ATTRIBUTES] 
                 sub C_FIELD_NAME  () { return "name"; }
                 sub C_FIELD_VALUE () { return "value"; }
                 sub C_FIELD_OLDVALUE () { return "oldvalue"; }
     sub C_DELETEDRECORDS () { return "deletedrecords"; }
#

# sql xml constants
# <document type="sql_xml">
  sub C_QUERY () { return "query"; }
#       [QUERY ATTRIBUTES] 
        sub C_QUERY_TYPE () { return "type"; }
#

# smart sql xml constants
# <document type="smart_sql">
  sub C_INSERT () { return "insert"; }
  sub C_UPDATE () { return "update"; }
  sub C_DELETE () { return "delete"; }
#

1;
