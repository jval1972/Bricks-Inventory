program brickinventory;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
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
  frm_editsetastext in 'frm_editsetastext.pas' {EditSetAsTextForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bricks Inventory';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTimingForm, TimingForm);
  Application.Run;
end.
