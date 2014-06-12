unit TimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, StdCtrls, Buttons, Grids, sqldb, db, MetaData, References, Connect;

type

  { TT }

  TT = class(TForm)
    LabelFilter: TLabel;
    LogicalConnective: TComboBox;
    FilterBtnPanel: TPanel;
    ShowFName: TCheckBox;
    FilterPanel: TPanel;
    Datasource: TDatasource;
    DrawGrid: TDrawGrid;
    BtnAddFilter: TSpeedButton;
    BtnExecuteFilter: TSpeedButton;
    yLabel: TLabel;
    XLabel: TLabel;
    ShowSched: TSpeedButton;
    SQLQuery: TSQLQuery;
    XCoord: TComboBox;
    YCoord: TComboBox;
    FieldPanel: TPanel;
    TakeFieldName: TCheckGroup;
    procedure BtnAddFilterClick(Sender: TObject);
    procedure BtnExecuteFilterClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ShowFNameChange(Sender: TObject);
    procedure ShowSchedClick;
    procedure TakeFieldNameItemClick(Sender: TObject; Index: integer);
  private
    Rec: array of array of array of array of string;
    x: array of string;
    y: array of string;
    xField, yField: TFieldInfo;
    xTable, yTable: TTableInfo;
    xIndex, yIndex: integer;
    FilterList: TFilterList;
    procedure DrawTitle(ACol, ARow: integer; ARect: TRect);
    procedure DrawCell(ACol, ARow: integer; ARect: TRect);
    procedure GetCheckGroupShowFieldsName;
    procedure GetXData;
    procedure GetYData;
    procedure FillCheckGroupTrue;
    procedure GetLogicalConnective;
    function CountTakeField: integer;
    function GetStrToShow(i, j, k, l: integer): string;
  const orderby: array[0..6] of string = ('NAME', 'NAME', 'NAME',
    'NAME', 'ID', 'NAME', 'NAME');
    { private declarations }
  public
    Table: TTableInfo;
    procedure ShowTimeTable;
    procedure TakeData(Ind: integer; f: boolean);
    procedure SelectRec;
    { public declarations }
  end;

var
  T: TT;

implementation

{$R *.lfm}

{ TT }

procedure TT.ShowSchedClick;
begin
  GetCheckGroupShowFieldsName;
  GetXData;
  GetYData;
  with DrawGrid do begin
    ColCount := Length(x) + 1;
    RowCount := Length(y) + 1;
    DefaultColWidth := 180;
    DefaultRowHeight := 180;
  end;
  SetLength(rec, 0, 0, 0 , 0);
  SelectRec;
  DrawGrid.Invalidate;
end;

procedure TT.TakeFieldNameItemClick(Sender: TObject; Index: integer);
begin
  DrawGrid.Invalidate;
end;

procedure TT.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i: integer;
begin
  if (aCol = 0) and (aRow = 0) then Exit;
  if (aCol = 0) xor (aRow = 0) then begin
    DrawTitle(aCol, aRow, aRect);
    Exit;
  end;
  DrawCell(aCol, aRow, aRect);
end;

procedure TT.BtnAddFilterClick(Sender: TObject);
begin
  if High(FilterList.Filters) > 14 then
    exit;
  FilterList.Add(-1, FilterPanel);
end;

procedure TT.BtnExecuteFilterClick(Sender: TObject);
begin
  ShowSchedClick;
end;

procedure TT.ShowFNameChange(Sender: TObject);
begin
   DrawGrid.Invalidate;
end;

procedure TT.DrawTitle(ACol, ARow: integer; ARect: TRect);
begin
  if aRow = 0 then
    DrawGrid.Canvas.TextOut(ARect.Left + 5, ARect.Top + 5, x[aCol - 1])
  else
    DrawGrid.Canvas.TextOut(ARect.Left + 5, ARect.Top + 2, y[aRow - 1]);
end;

procedure TT.DrawCell(ACol, ARow: integer; ARect: TRect);
var
  i, j, CountShow, Indent: integer;
  ShowLine: boolean;
begin
  CountShow := 0;
  Indent := 0;
  with DrawGrid.Canvas do begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    for i := 0 to High(Rec[ACol - 1, ARow - 1]) do begin

      if Rec[ACol - 1, ARow - 1, i, 0] = '' then Continue;

      for j := 0 to High(Rec[ACol - 1, ARow - 1, i]) do begin
        if (i <> 0) and (j = 0) and (CountShow <> 0) then inc(Indent, 10);
        if TakeFieldName.Checked[j] then begin
          ShowLine := true;
          TextOut(aRect.Left + 7, ARect.Top + CountShow * 19 + Indent,
            GetStrToShow(ACol - 1, ARow - 1, i, j));
        inc(CountShow);
        end;
      end;

      if ShowLine then begin
      inc(Indent, 10);
      Line(ARect.Left + 25, ARect.Top + CountShow * 19 + Indent,
        ARect.Right - 25, ARect.Top + CountShow * 19 + Indent);
      end;

    end;
  end;
end;

procedure TT.GetCheckGroupShowFieldsName;
var
  i: integer;
begin
  TakeFieldName := TCheckGroup.Create(FieldPanel);
  with TakeFieldName do begin
    Parent := FieldPanel;
    Left := 10;
    Top := 1;
    Width := 304;
    Height := 95;
    Columns := 3;
    Caption := 'Отобразить: ';
    OnItemClick := @TakeFieldNameItemClick;
  end;
  for i := 0 to High(Table.Fields) do
    TakeFieldName.Items.Add(Table.Fields[i].FieldCaption);
  FillCheckGroupTrue;
  TakeFieldName.Checked[0] := False;
end;

procedure TT.GetXData;
begin
  xIndex := XCoord.ItemIndex;
  TakeFieldName.Checked[xIndex + 1] := False;
  xField := Table.Fields[xIndex + 1];
  xTable := Meta[xIndex];
  TakeData(xIndex, true);
end;

procedure TT.GetYData;
begin
  yIndex := YCoord.ItemIndex;
  TakeFieldName.Checked[yIndex + 1] := False;
  yField := Table.Fields[yIndex + 1];
  yTable := Meta[yIndex];
  TakeData(yIndex, false);
end;

function TT.CountTakeField: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(Table.Fields) do
    if TakeFieldName.Checked[i] then inc(Result);
end;

function TT.GetStrToShow(i, j, k, l: integer): string;
begin
  if ShowFName.Checked then
    Result := Table.Fields[l].FieldCaption + ': ' + Rec[i, j, k, l]
  else
    Result := Rec[i, j, k, l];
end;

procedure TT.FillCheckGroupTrue;
var
  i: integer;
begin
  for i := 0 to High(Table.Fields) do
    TakeFieldName.Checked[i] := True;
end;

procedure TT.GetLogicalConnective;
begin
  with LogicalConnective do begin
    Items.Add('AND');
    Items.Add('OR');
    Caption := 'AND';
  end;
end;

procedure TT.ShowTimeTable;
var
  i: integer;
begin
  Caption := 'Расписание';
  for i := 1 to High(Table.Fields) - 1 do begin
    XCoord.Items.Add(Table.Fields[i].FieldCaption);
    YCoord.Items.Add(Table.Fields[i].FieldCaption);
  end;
  XCoord.Caption := Table.Fields[4].FieldCaption;
  YCoord.Caption := Table.Fields[5].FieldCaption;
  GetLogicalConnective;
  FilterList := TFilterList.Create(Table);
  ShowSchedClick;
  Show;
end;

procedure TT.TakeData(Ind: integer; f: boolean);
var
  A: array of string;
begin
  with SQLQuery do begin
    Close;
    SQL.Text := 'SELECT NAME FROM ' + Table.Fields[Ind + 1].KeyTable +
      ' ORDER BY ' + orderby[Ind];
    Open;
    First;
    while not EOF do begin
      SetLength(A, Length(A) + 1);
      A[High(A)] := SQLQuery.FieldByName('NAME').AsString;
      Next;
    end;
  end;
  if f then x := A else y := A;
end;

procedure TT.SelectRec;
var
  i, j, k, l: integer;
  wid, hei: integer;
  CondX, CondY: boolean;
begin
  wid := 50;
  hei := 50;
  k := 0;
  with SQLQuery do begin
    Close;
    SQL.Text := Format('%s ORDER BY %s.%s, %s.%s', [
      FilterList.GetQuery(LogicalConnective.Text), xTable.TableName,
      orderby[xIndex], yTable.TableName, orderby[yIndex]]);
    FilterList.SetParams(SQLQuery);
    Open;
    First;
    SetLength(Rec, Length(x), Length(y));
    i := 0;
    j := 0;
    while not EOF do begin
      CondX := Fields[xIndex + 1].AsString = x[i];
      CondY := Fields[yIndex + 1].AsString = y[j];
      if (j > High(x)) and (i > High(y)) then Exit;
      if CondX and CondY then begin
        inc(k);
        SetLength(Rec[i, j], k, High(Table.Fields) + 1);
        for l := 1 to High(Table.Fields) + 1 do
          Rec[i, j, k - 1, l - 1] := Fields[l - 1].AsString;
        Next;
      end else if CondX and not CondY then begin
        inc(j);
        k := 0;
      end else begin
            inc(i);
            j := 0;
          end;
    end;
  end;
end;

end.

