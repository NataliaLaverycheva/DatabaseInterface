unit References;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, DB, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, Buttons, StdCtrls, Connect, Editing, DBCtrls,
  MetaData;

type

  { TFilter }

  TFilter = class
    FilterPanel: TPanel;
    FieldsDBBox: TComboBox;
    CondBox: TComboBox;
    InField: TEdit;
    BtnDelete: TSpeedButton;
    constructor Create();
  end;

  { TFilterList }

  TFilterList = class
    Filters: array of TFilter;
    TableInfo: TTableInfo;
    function GetQuery(LogCon: string): string;
    procedure Add(AId: integer = -1; AFilterPanel: TPanel = nil);
    procedure SetParams(AQuery: TSQLQuery);
    function Where(ALogCon: string): String;
    procedure BtnDeleteFilterClick(Sender: TObject);
    constructor Create(ATableInfo: TTableInfo);
  end;

  { TReferences }

  TReferences = class(TForm)
    BtnChangeRec: TSpeedButton;
    BtnExecute: TSpeedButton;
    LogicalConnective: TComboBox;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    Panel: TPanel;
    BtnAddFilter: TSpeedButton;
    BtnDelete: TSpeedButton;
    BtnAddRec: TSpeedButton;
    FilterPanel: TPanel;
    SQLQuery: TSQLQuery;
    procedure BtnAddEditForm(Sender: TObject);
    procedure BtnAddFilterClick(Sender: TObject);
    procedure BtnExecuteClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure BtnDeleteClick(Sender: TObject);
  private
    FilterList: TFilterList;
    EditForms: array of TEditForm;
    procedure Refresh;
    function OrderBy: String;
    procedure DeleteRec;
    procedure UpdateData(Sender: TObject);
  public
    Id: integer;
    FSortIndex: Integer;
    FSortDir: boolean;
    Table: TTableInfo;
    procedure ShowTable(ATable: TTableInfo);
  end;

var
  FReferences: array of TReferences;

implementation

const
  Value: array[0..7] of string =
    ('Начинается с', 'Содержит подстроку',
    '<', '<=', '>', '>=', '=', '<>');

{$R *.lfm}

{ TReferences }

procedure TReferences.UpdateData(Sender: TObject);
begin
  SQLQuery.UpdateMode := UpWhereChanged;
  SQLQuery.ApplyUpdates;
  Connect.ConnectForm.SQLTransaction.Commit;
  Refresh;
end;

procedure TReferences.Refresh;
var
  t: String;
  i: Integer;
const
  SORT_INDICATOR: array [Boolean] of String = ('^', 'v');
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text :=
    Table.MakeQuery + FilterList.Where(LogicalConnective.Text) + OrderBy;
  FilterList.SetParams(SQLQuery);
  SQLQuery.Open;
  for i := 0 to High(Table.Fields) do begin
    t := Table.Fields[i].FieldCaption;
    if i = FSortIndex then
      t += ' ' + SORT_INDICATOR[FSortDir];
    DBGrid.Columns[i].Title.Caption := t;
    DBGrid.Columns[i].Width := Table.Fields[i].Width * 10;
  end;
end;

function TReferences.OrderBy: String;
var
  f: TFieldInfo;
begin
  f := Table.Fields[FSortIndex];
  Result := ' ORDER BY ';
  if f.KeyTable = '' then
    Result += f.FieldName
  else
    Result += Copy(f.FieldName, 1, Pos('_', f.FieldName) - 1) + 's.Name';
  if FSortDir then
    Result += ' DESC';
end;

procedure TReferences.DeleteRec;
var
  delId: integer;
begin
  if MessageDlg('Вы действительно хотите удалить запись?',
    mtConfirmation, mbYesNo, 0) <> mrYes
  then exit;
  delId := DBGrid.DataSource.DataSet.FieldByName('ID').AsInteger;
  with SQLQuery do begin
    Close;
    SQL.Text := Format('delete from %s where id = %d', [Table.TableName, delId]);
    ExecSQL;
    SQL.Text := Table.MakeQuery;
    Open;
  end;
  UpdateData(nil);
end;

procedure TReferences.ShowTable(ATable: TTableInfo);
begin
  FilterList := TFilterList.Create(ATable);
  Caption := ATable.TableCaption;
  Refresh;
  Show;
end;

procedure TReferences.BtnAddFilterClick(Sender: TObject);
begin
  if High(FilterList.Filters) > 14 then
    exit;
  FilterList.Add(Id);
end;

procedure TReferences.BtnAddEditForm(Sender: TObject);
var
  i, rId: Integer;
  isInsert: Boolean;
begin
  isInsert := (Sender as TSpeedButton).Tag = 1;
  if isInsert then
    rId := 0
  else begin
    rId := SQLQuery.FieldByName('id').AsInteger;
    for i:= 0 to High(EditForms) do
      if EditForms[i].Id = rId then begin
        EditForms[i].Show;
        Exit;
      end;
  end;
  SetLength(EditForms, Length(EditForms) + 1);
  EditForms[High(EditForms)] := TEditForm.Create(nil);
  EditForms[High(EditForms)].GetEditForm(Table, rId, @UpdateData);
end;

procedure TReferences.BtnExecuteClick(Sender: TObject);
begin
  Refresh;
end;

procedure TReferences.DBGridDblClick(Sender: TObject);
begin
  if DBGrid.MouseCoord(DBGrid.ScreenToClient(Mouse.CursorPos).x,
    DBGrid.ScreenToClient(Mouse.CursorPos).Y).Y = 0 then Exit;
  BtnAddEditForm(BtnChangeRec);
end;

procedure TReferences.DBGridTitleClick(Column: TColumn);
begin
  FSortIndex := Column.Index;
  FSortDir := not FSortDir;
  with SQLQuery do begin
    DisableControls;
    Self.Refresh;
    EnableControls;
  end;
end;

procedure TFilterList.BtnDeleteFilterClick(Sender: TObject);
var
  i, t: integer;
begin
  t := (Sender as TSpeedButton).Tag;
  FreeAndNil(Filters[t].FilterPanel);
  for i := t to High(Filters) - 1 do
    Filters[i] := Filters[i + 1];
  SetLength(Filters, Length(Filters) - 1);
end;

procedure TReferences.BtnDeleteClick(Sender: TObject);
begin
  DeleteRec;
end;

{ TFilter }

constructor TFilter.Create();
begin
end;

{ TFilterList }

function TFilterList.GetQuery(LogCon: string): string;
begin
  Result := TableInfo.MakeQuery + Where(LogCon);
end;

procedure TFilterList.Add(AId: integer; AFilterPanel: TPanel);
var
  v: String;
  j: Integer;
  ParentPanel, t: TPanel;
begin
  if AId = -1 then
    ParentPanel := AFilterPanel
  else
    ParentPanel := FReferences[AId].Panel;
  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)] := TFilter.Create();

  Filters[High(Filters)].FilterPanel := TPanel.Create(ParentPanel);
  with Filters[High(Filters)].FilterPanel do begin
    Parent := ParentPanel;
    Top := 500;
    Align := alTop;
    BevelOuter := bvNone;
  end;
  with ParentPanel do t := Filters[High(Filters)].FilterPanel;

  Filters[High(Filters)].InField := TEdit.Create(t);
  with Filters[High(Filters)].InField do begin
    Parent := t;
    Top := 11;
    Width := 70;
    Left := 270;
  end;

  Filters[High(Filters)].FieldsDBBox := TComboBox.Create(t);
  for j := 1 to High(TableInfo.Fields) do
    Filters[High(Filters)].FieldsDBBox.Items.Add(TableInfo.Fields[j].FieldCaption);
  with Filters[High(Filters)].FieldsDBBox do begin
    Style := csDropDownList;
    Caption := TableInfo.Fields[1].FieldCaption;
    Parent := t;
    Width := 115;
    Top := 11;
    Left := 10;
  end;

  Filters[High(Filters)].CondBox := TComboBox.Create(t);
  for v in Value do
    Filters[High(Filters)].CondBox.Items.Add(v);
  with Filters[High(Filters)].CondBox do begin
    Style := csDropDownList;
    Caption := Value[0];
    Parent := t;
    Top := 11;
    Width := 115;
    Left := 140;
  end;

  Filters[High(Filters)].BtnDelete := TSpeedButton.Create(t);
  with Filters[High(Filters)].BtnDelete do begin
    Caption := '-';
    Parent := t;
    Top := 11;
    Width := 30;
    Left := 400;
    Tag := High(Filters);
    OnClick := @BtnDeleteFilterClick;
  end;
end;

procedure TFilterList.SetParams(AQuery: TSQLQuery);
var
  i: Integer;
begin
  for i := 0 to High(Filters) do
    AQuery.ParamByName('par' + IntToStr(i)).AsString := Filters[i].InField.Text;
end;

function TFilterList.Where(ALogCon: string): String;
var
  i: integer;
  fname, p: string;
begin
  Result := '';
  for i := 0 to High(Filters) do begin
    with TableInfo.Fields[Filters[i].FieldsDBBox.ItemIndex + 1] do
      if KeyTable <> '' then
        fname := KeyTable + '.NAME'
      else
        fname := FieldName;
    p := ':par' + IntToStr(i);
    if Filters[i].CondBox.ItemIndex = 0 then
      Result += Format('%s LIKE %s || ''%%''', [fname, p])
    else if Filters[i].CondBox.ItemIndex = 1 then
      Result += Format('%s LIKE ''%%'' || %s || ''%%''', [fname, p])
    else if Filters[i].InField.Text <> '' then
      Result += Format('%s %s %s', [fname, Filters[i].CondBox.Text, p])
    else
      continue;
    Result += ' ' + ALogCon + ' ';
  end;
  Delete(Result, Length(Result) - 3, 3);
  if Result <> '' then
    Result := ' WHERE ' + Result;
end;

constructor TFilterList.Create(ATableInfo: TTableInfo);
begin
  TableInfo := ATableInfo;
end;

end.
