program CR_CRAWLER_SLOW;

uses
  FastMM4 in 'FastMM4.pas',
  Forms,
  CR_timing_slow in 'CR_timing_slow.pas' {Form1},
  bi_priceadjust in 'bi_priceadjust.pas',
  bi_io in 'bi_io.pas',
  bi_binaryset in 'bi_binaryset.pas',
  bi_globals in 'bi_globals.pas',
  bi_db in 'bi_db.pas',
  bi_binarypart in 'bi_binarypart.pas',
  bi_crawler in 'bi_crawler.pas',
  bi_currency in 'bi_currency.pas',
  bi_delphi in 'bi_delphi.pas',
  bi_hash in 'bi_hash.pas',
  bi_hash512 in 'bi_hash512.pas',
  bi_multithread in 'bi_multithread.pas',
  bi_pak in 'bi_pak.pas',
  bi_script in 'bi_script.pas',
  bi_system in 'bi_system.pas',
  bi_threads in 'bi_threads.pas',
  bi_threadtimer in 'bi_threadtimer.pas',
  bi_tmp in 'bi_tmp.pas',
  bi_utils in 'bi_utils.pas',
  bi_zip in 'bi_zip.pas',
  GIFImage in 'GIFImage.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  zlibpas in 'zlibpas.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bricklink Crawler (Slow)';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
