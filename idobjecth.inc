{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!

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

  TIDType = LongInt;

  TIDObject = class(TObject)
  private
     FID : TIDType;
  public
     constructor Create;
     constructor Create(AID : TIDType);
     
     property ID : TIDType read FID write FID;
  end;
