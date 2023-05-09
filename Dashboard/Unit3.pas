unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, uSuperChartLight;

type
  TForm3 = class(TForm)
    Layout1: TLayout;
    Label2: TLabel;
    img_menu: TImage;
    img_refresh: TImage;
    Label1: TLabel;
    layout_chart: TLayout;
    layout_chart2: TLayout;
    Layout3: TLayout;
    Rectangle1: TRectangle;
    Label3: TLabel;
    Rectangle2: TRectangle;
    Label4: TLabel;
    Line1: TLine;
    procedure img_refreshClick(Sender: TObject);
  private
    procedure MontaGrafico1;
    procedure MontaGrafico2;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}


procedure TForm3.MontaGrafico1;
var
    erro, jsonStr: string;
    chart : TSuperChart;
begin
    try
        chart := TSuperChart.Create(layout_chart, Lines);

        // Valores
        chart.ShowValues := true;
        chart.FontSizeValues := 12;
        chart.FontColorValues := $FF999999;
        chart.FormatValues := '#,##0.00';

        // Barras
        //chart.BarColor := $FFFFD270;
        //chart.ShowBackground := true;
        //chart.BackgroundColor := $FF4F5060;
        //chart.RoundedBotton := true;
        //chart.RoundedTop := true;
        chart.LineColor := $FF7DACEF;

        // Arguments
        chart.FontSizeArgument := 13;
        chart.FontColorArgument := $FF878787;

        // JSON
        jsonStr := '[{"field":"Sem 1", "valor":520}, {"field":"Sem 2", "valor":580},';
        jsonStr := jsonStr + '{"field":"Sem 3", "valor":600}, {"field":"Sem 4", "valor":700}]';

        chart.LoadFromJSON(jsonStr, erro);

        // Tratamento de erro
        if erro <> '' then
            showmessage(erro);
    finally
        chart.DisposeOf;
    end;
end;

procedure TForm3.MontaGrafico2;
var
    erro, jsonStr: string;
    chart : TSuperChart;
begin
    try
        chart := TSuperChart.Create(layout_chart2, Lines);

        // Valores
        chart.ShowValues := true;
        chart.FontSizeValues := 12;
        chart.FontColorValues := $FF999999;
        chart.FormatValues := '#,##0.00';

        // Barras
        //chart.BarColor := $FFFFD270;
        //chart.ShowBackground := true;
        //chart.BackgroundColor := $FF4F5060;
        //chart.RoundedBotton := true;
        //chart.RoundedTop := true;
        chart.LineColor := $FFFFD270;

        // Arguments
        chart.FontSizeArgument := 13;
        chart.FontColorArgument := $FF878787;

        // JSON
        jsonStr := '[{"field":"Sem 1", "valor":920}, {"field":"Sem 2", "valor":480},';
        jsonStr := jsonStr + '{"field":"Sem 3", "valor":720}, {"field":"Sem 4", "valor":930}]';

        chart.LoadFromJSON(jsonStr, erro);

        // Tratamento de erro
        if erro <> '' then
            showmessage(erro);
    finally
        chart.DisposeOf;
    end;
end;

procedure TForm3.img_refreshClick(Sender: TObject);
begin
    MontaGrafico1;
    MontaGrafico2;
end;

end.
