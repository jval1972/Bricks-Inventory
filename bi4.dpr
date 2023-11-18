//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2021 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//    Main Program
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

program bi4;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Fastcode in 'FASTCODE\FastCode.pas',
  AnsiStringReplaceJOHIA32Unit12 in 'FASTCODE\AnsiStringReplaceJOHIA32Unit12.pas',
  AnsiStringReplaceJOHPASUnit12 in 'FASTCODE\AnsiStringReplaceJOHPASUnit12.pas',
  FastcodeAnsiStringReplaceUnit in 'FASTCODE\FastcodeAnsiStringReplaceUnit.pas',
  FastcodeCompareMemUnit in 'FASTCODE\FastcodeCompareMemUnit.pas',
  FastcodeCompareStrUnit in 'FASTCODE\FastcodeCompareStrUnit.pas',
  FastcodeCompareTextUnit in 'FASTCODE\FastcodeCompareTextUnit.pas',
  FastcodeCPUID in 'FASTCODE\FastcodeCPUID.pas',
  FastcodeFillCharUnit in 'FASTCODE\FastcodeFillCharUnit.pas',
  FastcodeLowerCaseUnit in 'FASTCODE\FastcodeLowerCaseUnit.pas',
  FastcodePatch in 'FASTCODE\FastcodePatch.pas',
  FastcodePosExUnit in 'FASTCODE\FastcodePosExUnit.pas',
  FastcodePosUnit in 'FASTCODE\FastcodePosUnit.pas',
  FastcodeStrCompUnit in 'FASTCODE\FastcodeStrCompUnit.pas',
  FastcodeStrCopyUnit in 'FASTCODE\FastcodeStrCopyUnit.pas',
  FastcodeStrICompUnit in 'FASTCODE\FastcodeStrICompUnit.pas',
  FastCodeStrLenUnit in 'FASTCODE\FastCodeStrLenUnit.pas',
  FastcodeStrToInt32Unit in 'FASTCODE\FastcodeStrToInt32Unit.pas',
  FastcodeUpperCaseUnit in 'FASTCODE\FastcodeUpperCaseUnit.pas',
  FastMove in 'FASTCODE\FastMove.pas',
  Forms,
  main in 'main.pas' {MainForm},
  bi_delphi in 'bi_delphi.pas',
  bi_pak in 'bi_pak.pas',
  bi_zip in 'bi_zip.pas',
  bi_io in 'bi_io.pas',
  bi_system in 'bi_system.pas',
  bi_tmp in 'bi_tmp.pas',
  bi_db in 'bi_db.pas',
  bi_script in 'bi_script.pas',
  slpash in 'slpash.pas' {SplashForm},
  bi_docwriter in 'bi_docwriter.pas',
  bi_utils in 'bi_utils.pas',
  bi_hash in 'bi_hash.pas',
  bi_crawler in 'bi_crawler.pas',
  timing in 'timing.pas' {TimingForm},
  searchset in 'searchset.pas' {SearchSetForm},
  frm_multiplesets in 'frm_multiplesets.pas' {MultipleSetsForm},
  bl_orderxml in 'bl_orderxml.pas',
  bi_orders in 'bi_orders.pas',
  bi_threads in 'bi_threads.pas',
  PreviewForm in 'PreviewForm.pas' {PreviewForm},
  PrintStatusForm in 'PrintStatusForm.pas' {PrnStatusForm},
  Gopage in 'Gopage.pas' {GoPageForm},
  bi_currency in 'bi_currency.pas',
  compare2sets in 'compare2sets.pas' {frmCompare2Sets},
  mosaicfrm in 'mosaicfrm.pas' {MosaicForm},
  searchpart in 'searchpart.pas' {SearchPartForm},
  editpiecefrm in 'editpiecefrm.pas' {EditPieceForm},
  bi_threadtimer in 'bi_threadtimer.pas',
  frm_diagrams in 'frm_diagrams.pas' {DiagramForm},
  GIFImage in 'GIFImage.pas',
  frm_lugbulksuggest in 'frm_lugbulksuggest.pas' {LugbulkSuggestForm},
  bi_priceadjust in 'bi_priceadjust.pas',
  frm_selectsets in 'frm_selectsets.pas' {SelectSetsForm},
  frm_batch in 'frm_batch.pas' {BatchLinkForm},
  removepiecefromstoragefrm in 'removepiecefromstoragefrm.pas' {RemovePieceFromStorageForm},
  bi_binaryset in 'bi_binaryset.pas',
  bi_globals in 'bi_globals.pas',
  frm_editsetastext in 'frm_editsetastext.pas' {EditSetAsTextForm},
  bi_hash512 in 'bi_hash512.pas',
  bi_lugbulk2017 in 'bi_lugbulk2017.pas',
  ImportFileForm in 'ImportFileForm.pas' {ImportFileForm},
  frm_setsforpartout_params in 'frm_setsforpartout_params.pas' {QueryPartOutParametersForm},
  mosaicfrm_plates in 'mosaicfrm_plates.pas' {MosaicFormPlates},
  mosaicfrm_tiles in 'mosaicfrm_tiles.pas' {MosaicFormTiles},
  searchstorage in 'searchstorage.pas' {SearchStorageForm},
  frm_setsminifigspartout_params in 'frm_setsminifigspartout_params.pas' {QueryMinifigPartOutParametersForm},
  editmoldfrm in 'editmoldfrm.pas' {EditMoldForm},
  frm_update1 in 'frm_update1.pas' {UpdateForm1},
  frm_update2 in 'frm_update2.pas' {UpdatePartNamesRebrForm},
  frm_update3 in 'frm_update3.pas' {TUpdatePartColorBLForm},
  frm_update4 in 'frm_update4.pas' {TUpdateNewPartsFromBLForm},
  bi_binarypart in 'bi_binarypart.pas',
  bi_multithread in 'bi_multithread.pas',
  bi_crc32 in 'bi_crc32.pas',
  bi_iterators in 'bi_iterators.pas',
  frm_options in 'frm_options.pas' {OptionsForm},
  bi_defs in 'bi_defs.pas',
  frm_selectparttype in 'frm_selectparttype.pas' {SelectPartTypeForm},
  bi_data in 'bi_data.pas',
  frm_editlugbulkprice in 'frm_editlugbulkprice.pas' {EditLugbulkPriceForm},
  bi_cachefile in 'bi_cachefile.pas',
  frm_editbllink in 'frm_editbllink.pas' {EditBricklinkLinkForm},
  bi_pghistory in 'bi_pghistory.pas',
  bi_instructions in 'bi_instructions.pas',
  bi_pdf2jpeg in 'bi_pdf2jpeg.pas',
  bi_wzipfile in 'bi_wzipfile.pas',
  frm_pdfinstructions in 'frm_pdfinstructions.pas' {ImportPDFForm},
  bi_imagerotate in 'bi_imagerotate.pas',
  buildinexcludes in 'buildinexcludes.pas',
  bi_readylist in 'bi_readylist.pas',
  bi_storage in 'bi_storage.pas',
  DitherUnit in 'DitherUnit.pas',
  FramBrwz in 'FramBrwz.pas',
  FramView in 'framview.pas',
  GDIPL2A in 'GDIPL2A.pas',
  HtmlGif1 in 'htmlgif1.pas',
  HTMLGif2 in 'HTMLGif2.pas',
  Htmlsbs1 in 'Htmlsbs1.pas',
  Htmlsubs in 'htmlsubs.pas',
  HTMLUn2 in 'HTMLUn2.pas',
  Htmlview in 'htmlview.pas',
  MetaFilePrinter in 'MetaFilePrinter.pas',
  pngextra in 'pngextra.pas',
  pngimage in 'pngimage.pas',
  PngImage1 in 'PngImage1.pas',
  pnglang in 'pnglang.pas',
  PNGZLIB1 in 'PNGZLIB1.pas',
  Readhtml in 'Readhtml.pas',
  StylePars in 'StylePars.pas',
  StyleUn in 'StyleUn.pas',
  URLSubs in 'URLSubs.pas',
  vwPrint in 'vwPrint.pas',
  zlibpas in 'zlibpas.pas',
  bi_quantize in 'bi_quantize.pas',
  bi_4gb in 'bi_4gb.pas',
  bi_description_compress in 'bi_description_compress.pas',
  bi_dimensions in 'bi_dimensions.pas',
  HTMLEd1 in 'HTMLEd1.pas' {EditHtmlForm},
  bi_colorpickerbutton in 'bi_colorpickerbutton.pas',
  bi_dropdownbutton in 'bi_dropdownbutton.pas',
  bi_binary in 'bi_binary.pas',
  bi_undo in 'bi_undo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bricks Inventory';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTimingForm, TimingForm);
  Application.CreateForm(TImportPDFForm, ImportPDFForm);
  Application.Run;
end.
