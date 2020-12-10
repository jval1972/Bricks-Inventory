program CR_CRAWLER;

uses
  FastMM4 in 'FastMM4.pas',
  Forms,
  CR_timing in 'CR_timing.pas' {Form1},
  bi_priceadjust in 'bi_priceadjust.pas',
  bi_io in 'bi_io.pas',
  bi_binaryset in 'bi_binaryset.pas',
  bi_globals in 'bi_globals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bricklink Crawler';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
