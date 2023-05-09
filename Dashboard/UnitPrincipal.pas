unit UnitPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.JSON,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,

  uSuperChartLight, FMX.Ani;

type
  TFrmPrincipal = class(TForm)
    Layout2: TLayout;
    Label2: TLabel;
    img_refresh: TImage;
    VertScrollBox1: TVertScrollBox;
    Rectangle1: TRectangle;
    layout_chart1: TLayout;
    Rectangle2: TRectangle;
    layout_chart2: TLayout;
    Rectangle3: TRectangle;
    layout_chart3: TLayout;
    Layout1: TLayout;
    Label1: TLabel;
    Label3: TLabel;
    ArcBase1: TArc;
    ArcValor1: TArc;
    lblValor1: TLabel;
    AnimationArc1: TFloatAnimation;
    ArcBase2: TArc;
    ArcValor2: TArc;
    lblValor2: TLabel;
    AnimationArc2: TFloatAnimation;
    procedure img_refreshClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    procedure MontarGrafico1;
    procedure MontarGraficos;
    procedure MontarGrafico2;
    procedure MontarGrafico3;
    procedure MontarGrafico4;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.fmx}

uses Unit2, Unit3;

procedure TFrmPrincipal.MontarGraficos;
begin
    MontarGrafico1;

    TThread.CreateAnonymousThread(procedure
    begin
        sleep(500);

        TThread.Synchronize(nil, MontarGrafico2);
    end).Start;

    TThread.CreateAnonymousThread(procedure
    begin
        sleep(500);

        TThread.Synchronize(nil, MontarGrafico3);
    end).Start;

    TThread.CreateAnonymousThread(procedure
    begin
        sleep(500);

        TThread.Synchronize(nil, MontarGrafico4);
    end).Start;
end;

procedure TFrmPrincipal.Label1Click(Sender: TObject);
begin
    Form2.Show;
end;

procedure TFrmPrincipal.Label3Click(Sender: TObject);
begin
    Form3.Show;
end;

procedure TFrmPrincipal.MontarGrafico1;
var
    chart: TSuperChart;
    jsonStr, erro: string;
begin
    try
        chart := TSuperChart.Create(layout_chart1, Lines);

        // Valores...
        chart.ShowValues := true;
        chart.FontSizeValues := 10;
        chart.FontColorValues := $FFFFFFFF;
        chart.FormatValues := '#,##0.00';


        // Linhas...
        chart.LineColor := $FFFFFFFF;
        //chart.ShowBackground := true;
        //chart.BackgroundColor := $FF4F5060;
        //chart.RoundedBotton := true;
        //chart.RoundedTop := true;

        // Argumentos...
        chart.FontSizeArgument := 9;
        chart.FontColorArgument := $FFFFFFFF;

        // Json...
        jsonStr := '[{"field":"Jan", "valor":220}, {"field":"Fev", "valor":580}, {"field":"Mar", "valor":750},';
        jsonStr := jsonStr + '{"field":"Abr", "valor":600}, {"field":"Mai", "valor":700}, {"field":"Jun", "valor":800},';
        jsonStr := jsonStr + '{"field":"Jul", "valor":950}, {"field":"Ago", "valor":1100}, {"field":"Set", "valor":950},';
        jsonStr := jsonStr + '{"field":"Out", "valor":800}, {"field":"Nov", "valor":700}, {"field":"Dez", "valor":900}]';

        // Render do grafico...
        chart.LoadFromJSON(jsonStr, erro);

        if NOT erro.IsEmpty then
            showmessage(erro);

    finally
        chart.DisposeOf;
    end;
end;

procedure TFrmPrincipal.MontarGrafico2;
var
    chart: TSuperChart;
    jsonStr, erro: string;
begin
    try
        chart := TSuperChart.Create(layout_chart2, Lines);

        // Valores...
        chart.ShowValues := true;
        chart.FontSizeValues := 11;
        chart.FontColorValues := $FF6C6C6C;
        chart.FormatValues := '';


        // Linhas...
        chart.LineColor := $FF487DF7;


        // Argumentos...
        chart.FontSizeArgument := 11;
        chart.FontColorArgument := $FF6C6C6C;


        // Json...
        jsonStr := '[{"field":"Jan", "valor":520}, {"field":"Fev", "valor":400}, {"field":"Mar", "valor":840},';
        jsonStr := jsonStr + '{"field":"Abr", "valor":200}, {"field":"Mai", "valor":997}, {"field":"Jun", "valor":1270}]';


        // Render do grafico...
        chart.LoadFromJSON(jsonStr, erro);

        if NOT erro.IsEmpty then
            showmessage(erro);

    finally
        chart.DisposeOf;
    end;
end;


// Gauge...
procedure TFrmPrincipal.MontarGrafico3;
var
    vl_previsto, vl_realizado: double;
begin
    vl_previsto := 10000;
    vl_realizado := 7200;

    lblValor1.Text := FormatFloat('#,##0.00', vl_realizado);

    //ArcValor1.EndAngle := (vl_realizado / vl_previsto) * 360;
    AnimationArc1.StopValue := (vl_realizado / vl_previsto) * 360;
    AnimationArc1.Start;
end;

procedure TFrmPrincipal.MontarGrafico4;
var
    vl_previsto, vl_realizado: double;
begin
    vl_previsto := 50000;
    vl_realizado := 12500;

    lblValor2.Text := FormatFloat('0.00%', vl_realizado / vl_previsto * 100);

    AnimationArc2.StopValue := (vl_realizado / vl_previsto) * 360;
    AnimationArc2.Start;
end;


procedure TFrmPrincipal.img_refreshClick(Sender: TObject);
begin
    MontarGraficos;
end;

end.
