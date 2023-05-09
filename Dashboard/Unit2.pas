unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, uSuperChartLight,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, System.JSON, System.IOUtils,

  DataSet.Serialize;

type
  TForm2 = class(TForm)
    Layout1: TLayout;
    img_menu: TImage;
    layout_chart: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    img_refresh: TImage;
    conn: TFDConnection;
    qry: TFDQuery;
    procedure img_refreshClick(Sender: TObject);
  private
    procedure MontaGrafico;
    procedure ConectarBanco;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.ConectarBanco;
begin
    with Conn do
    begin
        {$IFDEF MSWINDOWS}
        try
            if NOT FileExists(System.SysUtils.GetCurrentDir + '\DB\banco.db') then
            begin
                showmessage('Banco não encontrado: ' + System.SysUtils.GetCurrentDir + '\DB\banco.db');
                exit;
            end;

            Params.Values['Database'] := System.SysUtils.GetCurrentDir + '\DB\banco.db';
            Connected := true;
        except on E:Exception do
                raise Exception.Create('Erro de conexão com o banco de dados: ' + E.Message);
        end;

        {$ELSE}

        Params.Values['DriverID'] := 'SQLite';
        try
            Params.Values['Database'] := TPath.Combine(TPath.GetDocumentsPath, 'banco.db');
            Connected := true;
        except on E:Exception do
            raise Exception.Create('Erro de conexão com o banco de dados: ' + E.Message);
        end;
        {$ENDIF}
    end;
end;


procedure TForm2.MontaGrafico;
var
    erro: string;
    chart : TSuperChart;
    jsonArray: TJSONArray;
begin
    try
        ConectarBanco;

        chart := TSuperChart.Create(layout_chart, Lines);

        // Valores
        chart.ShowValues := true;
        chart.FontSizeValues := 12;
        chart.FontColorValues := $FF999999;
        chart.FormatValues := '#,##0.00';

        // Barras
        chart.BarColor := $FFFFD270;
        chart.ShowBackground := true;
        chart.BackgroundColor := $FF4F5060;
        chart.RoundedBotton := true;
        chart.RoundedTop := true;
        //chart.LineColor := $FFFFD270;

        // Arguments
        chart.FontSizeArgument := 13;
        chart.FontColorArgument := $FF878787;



        // JSON
        qry.Active := false;
        qry.SQL.Clear;
        qry.SQL.Add('select mes as field, valor from tab_atividades order by ordem');
        qry.Active := true;


        jsonArray := qry.ToJSONArray;

        //jsonStr := '[{"field":"Jan", "valor":520}, {"field":"Fev", "valor":580}, {"field":"Mar", "valor":750},';
        //jsonStr := jsonStr + '{"field":"Abr", "valor":600}, {"field":"500", "valor":700}, {"field":"Jun", "valor":800},';
        //jsonStr := jsonStr + '{"field":"Jul", "valor":950}, {"field":"Ago", "valor":1100}, {"field":"Set", "valor":950},';
        //jsonStr := jsonStr + '{"field":"Out", "valor":800}, {"field":"Nov", "valor":700}, {"field":"Dez", "valor":900}]';

        chart.LoadFromJSON(jsonArray.ToJSON, erro);

        jsonArray.DisposeOf;

        // Tratamento de erro
        if erro <> '' then
            showmessage(erro);
    finally
        chart.DisposeOf;
    end;
end;

procedure TForm2.img_refreshClick(Sender: TObject);
begin
    MontaGrafico;
end;

end.
