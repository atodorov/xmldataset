{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!
 Includes some globally used defines.

 (c) by Alexander Todorov.
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

// fpc 2.0.0
{$IF ((FPC_VERSION = 2) and (FPC_RELEASE = 0) and (FPC_PATCH = 0))}
   {$DEFINE FPC_VER_200}
{$ENDIF}

(* fpc 2.0.1 *)
(* TDataset.TempBuffer is not present in FPC 2.0.1. It is present in 2.0.0 and 2.0.1+*)
(* It is implemented by TGXBaseDataSet *)
{$IF ((FPC_VERSION = 2) and (FPC_RELEASE = 0) and (FPC_PATCH = 1))}
   {$DEFINE FPC_VER_201}
   {$WARNINGS ON}
{$ENDIF}

// fpc >= 2.0.1
{$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 0) and (FPC_PATCH >= 1))}
   {$DEFINE FPC_VER_201+}
{$ENDIF}

(* a workaround widestrings. We use this because DOMString = WideString *)
{$DEFINE USE_WIDESTRINGS}

(* a hack for LargeInt := Variant ==> LargeInt := StrToInt64(Variant) *)
(* EVarianterror ###(gdb unparsed remainder: s $0 out of bounds>)###) *)
{$DEFINE USE_STR_TO_INT64}
// the same reason
{$DEFINE USE_STR_TO_FLOAT}

// use synapse tcp / ip
{$IFDEF WIN32}
  {$DEFINE USE_SYNAPSE}
{$ENDIF}
