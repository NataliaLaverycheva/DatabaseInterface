unit TimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, StdCtrls, Buttons, Grids, sqldb, db, MetaData, References, Connect,
  Editing;

type

  { TEditPanel }

  TEditPanel = class
    Panel: array of TPanel;
    BtnDelete, BtnInsert, BtnUpdate: TSpeedButton;
    procedure GetEditPanel(ADrawGrid: TDrawGrid; ACol, ARow: integer);
  end;

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
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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
    EditPanel: TEditPanel;
    ColEdit, RowEdit: integer;
    EditForm: TEditForm;
    procedure DrawTitle(ACol, ARow: integer; ARect: TRect);
    procedure DrawCell(ACol, ARow: integer; ARect: TRect);
    procedure GetCheckGroupShowFieldsName;
    procedure GetXData;
    procedure GetYData;
    procedure FillCheckGroupTrue;
    procedure GetLogicalConnective;
    function CountTakeField: integer;
    function GetStrToShow(i, j, k, l: integer): string;
    procedure DeleteDataFromCell(Sender: TObject);
    procedure InsertDataAtCell(Sender: TObject);
    procedure UpdateDataAtCell(Sender: TObject);
    procedure ShowCell(ACol, ARow: integer);
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

{ TEditPanel }

procedure TEditPanel.GetEditPanel(ADrawGrid: TDrawGrid; ACol, ARow: integer);
var
  i, Id: integer;
  Rect: TRect;
begin
  Rect := ADrawGrid.CellRect(ACol, ARow);
  for i := 0 to High(Panel) do begin
    Id := StrToInt(T.Rec[ACol - 1, ARow - 1, i, 0]);
    with Panel[i] do begin
      Parent := ADrawGrid;
      Height := 25;
      Width := 67;
      Top := Rect.Top + (i + 1)*110;
      Left := Rect.Left + 120;
      BevelOuter := bvNone;
    end;
    BtnDelete := TSpeedButton.Create(Panel[i]);
    with BtnDelete do begin
      Parent := Panel[i];
      Tag := Id;
      Left := 3;
      Glyph.LoadFromFile(GetCurrentDir + '\ico\deletered_2343.bmp');
      Height := 20;
      Width := 20;
      OnClick := @T.DeleteDataFromCell;
    end;
    BtnInsert := TSpeedButton.Create(Panel[i]);
    with BtnInsert do begin
      Parent := Panel[i];
      Left := 25;
      Glyph.LoadFromFile(GetCurrentDir + '\ico\edit_add_5036.bmp');
      Height := 20;
      Width := 20;
      OnClick := @T.InsertDataAtCell;
    end;
    BtnUpdate := TSpeedButton.Create(Panel[i]);
    with BtnUpdate do begin
      Parent := Panel[i];
      Tag := Id;
      Left := 47;
      Glyph.LoadFromFile(GetCurrentDir + '\ico\noatunloopsong_6487.bmp');
      Height := 20;
      Width := 20;
      OnClick := @T.UpdateDataAtCell;
    end;
  end;
end;

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
    DefaultColWidth := 200;
    DefaultRowHeight := 150;
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
begin
  if (aCol = 0) and (aRow = 0) then Exit;
  if (aCol = 0) xor (aRow = 0) then begin
    DrawTitle(aCol, aRow, aRect);
    Exit;
  end;
  DrawCell(aCol, aRow, aRect);
end;

procedure TT.DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: integer;
  Rect: TRect;
begin
  DrawGrid.MouseToCell(x, y, Col, Row);
  Rect := DrawGrid.CellRect(Col, Row);
  if (Col = 0) or (Row = 0) then Exit;
  if (x > Rect.BottomRight.x - 15) and (x < Rect.BottomRight.x) and
  (y > Rect.BottomRight.y - 15) and (y < Rect.BottomRight.y) then
    ShowCell(Col, Row);
end;

procedure TT.DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Col, Row, i: integer;
  Rect: TRect;
begin
  Col := 0;
  Row := 0;
  DrawGrid.MouseToCell(x, y, Col, Row);
  for i := 0 to High(EditPanel.Panel) do
    FreeAndNil(EditPanel.Panel[i]);
  SetLength(EditPanel.Panel, 0);
  if (Col = 0) or (Row = 0) then Exit;
  ColEdit := Col;
  RowEdit := Row;
  SetLength(EditPanel.Panel, Length(Rec[Col - 1, Row - 1]));
  for i := 0 to High((Rec[Col - 1, Row - 1])) do
    EditPanel.Panel[i] := TPanel.Create(DrawGrid);
  EditPanel.GetEditPanel(DrawGrid, Col, Row);
end;

procedure TT.BtnAddFilterClick(Sender: TObject);
begin
  if High(FilterList.Filters) > 14 then
    exit;
  FilterList.Add(FilterPanel);
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
  i, j, NowHeight, CellHeight, CountStrShow, Indent: integer;
begin
  CountStrShow := 0;
  Indent := 0;
  CellHeight := DrawGrid.RowHeights[ARow] - 25;
  with DrawGrid.Canvas do begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    for i := 0 to High(Rec[ACol - 1, ARow - 1]) do begin

      for j := 0 to High(Rec[ACol - 1, ARow - 1, i]) do begin
        if TakeFieldName.Checked[j] then begin
          NowHeight := CountStrShow * 19 + Indent;
          if NowHeight > CellHeight then Break;
          TextOut(aRect.Left + 7, ARect.Top + CountStrShow * 19 + Indent,
            GetStrToShow(ACol - 1, ARow - 1, i, j));
          inc(CountStrShow);
        end;
      end;

      if NowHeight > CellHeight then Break;
      inc(Indent, 10);
      Line(
        ARect.Left + 25, ARect.Top + CountStrShow * 19 + Indent,
        ARect.Right - 25, ARect.Top + CountStrShow * 19 + Indent);
      inc(Indent, 10);
    end;

    if Length(Rec[ACol - 1, ARow - 1]) > 1 then
      Rectangle(ARect.BottomRight.x - 15, ARect.BottomRight.y - 15,
        ARect.BottomRight.x, ARect.BottomRight.y);
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

procedure TT.DeleteDataFromCell(Sender: TObject);
var
  i: integer;
begin
  if MessageDlg('Вы действительно хотите удалить все записи в этой ячейке?',
    mtConfirmation, mbYesNo, 0) <> mrYes then exit;
  for i := 0 to High(Rec[ColEdit - 1, RowEdit - 1]) do
    DeleteRec(SQLQuery, StrToInt(Rec[ColEdit - 1, RowEdit - 1, i, 0]), Table);
  SQLQuery.ApplyUpdates;
  Connect.ConnectForm.SQLTransaction.Commit;
  ShowSchedClick;
end;

procedure TT.InsertDataAtCell(Sender: TObject);
begin
  EditForm := TEditForm.Create(nil);
  EditForm.GetEditForm(Table, 0, nil);
  EditForm.Show;
end;

procedure TT.UpdateDataAtCell(Sender: TObject);
begin

end;

procedure TT.ShowCell(ACol, ARow: integer);
begin
  DrawGrid.RowHeights[ARow] := Length(Rec[ACol - 1, ARow - 1])*170;
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
  EditPanel := TEditPanel.Create;
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
  CondX, CondY: boolean;
begin
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
      if CondX and CondY then begin
        inc(k);
        SetLength(Rec[i, j], k, High(Table.Fields) + 1);
        for l := 1 to High(Table.Fields) + 1 do
          Rec[i, j, k - 1, l - 1] := Fields[l - 1].AsString;
        Next;
      end else begin
        if CondX and not CondY then
          inc(j)
        else begin
          inc(i);
          j := 0;
        end;
        k := 0;
      end;
    end;
  end;
end;

end.

