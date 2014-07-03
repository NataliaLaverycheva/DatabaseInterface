unit Editing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
    StdCtrls, DbCtrls, ExtCtrls, MetaData, DBGrids, db, sqldb, Connect;

type

  TEditField = class
    FieldCaption: TLabel;
    Text: TEdit;
    Box: TDBLookupComboBox;
  end;

  { TEditForm }

  TEditForm = class(TForm)
    BtnDel: TSpeedButton;
    BtnSave: TSpeedButton;
    Datasource: TDatasource;
    Panel: TPanel;
    SQLQuery: TSQLQuery;
    procedure BtnDelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure DatasourceDataChange(Sender: TObject; Field: TField);
  private
    TableInfo: TTableInfo;
    rec: array of TEditField;
    FOnChange: TNotifyEvent;
    function InsertQuery: String;
    function UpdateQuery: String;
    procedure DeleteQuotes(var s: string);
    procedure SetParams(AQuery: TSQLQuery);
  public
    Id: integer;
    procedure GetEditForm(
      ATable: TTableInfo; AId: integer; AOnChange: TNotifyEvent);
  end;

implementation

uses References;

{$R *.lfm}

{ TEditForm }


procedure TEditForm.GetEditForm(ATable: TTableInfo;
  AId: integer; AOnChange: TNotifyEvent);
var
  i: integer;
  LookupQuery: TSQLQuery;
  LookupDataSource: TDataSource;
  s, AFieldName, fn: string;
begin
  Id := AId;
  TableInfo := ATable;
  FOnChange:= AOnChange;
  Caption := TableInfo.TableCaption;
  SQLQuery.DataBase := ConnectForm.IBConnection;
  SQLQuery.Transaction := ConnectForm.SQLTransaction;
  SQLQuery.SQL.Text := TableInfo.MakeQuery(true) +
    ' WHERE ' + TableInfo.TableName + '.id = :id';
  SQLQuery.ParamByName('id').AsInteger := Id;
  SQLQuery.Open;
  SQLQuery.Edit;

  SetLength(rec, Length(TableInfo.Fields));
  for i := 1 to high(TableInfo.Fields) do begin
    fn := TableInfo.Fields[i].FieldName;
    DeleteQuotes(fn);
    rec[i] := TEditField.Create;

    rec[i].FieldCaption := TLabel.Create(Self.Panel);
    with rec[i].FieldCaption do begin
      parent := Self.Panel;
      Caption := TableInfo.Fields[i].FieldCaption;
      Top := i * 40;
      Width := 115;
      Left := 70;
    end;

    if TableInfo.Fields[i].KeyField = '' then begin
      rec[i].Text := TEdit.Create(Self.Panel);
      with rec[i].Text do begin
        s := SQLQuery.FieldByName(fn).AsString;
        if Id > 0 then
          Text := SQLQuery.FieldByName(fn).AsString;
        Parent := Self.Panel;
        Top := i * 40;
        Width := 115;
        Left := 210;
        ReadOnly := False;
      end;
    end else begin
      AFieldName := TableInfo.Fields[i].KeyField;
      DeleteQuotes(AFieldName);
      LookupQuery := TSQLQuery.Create(Self);
      with LookupQuery do begin
        DataBase := ConnectForm.IBConnection;
        Transaction := ConnectForm.SQLTransaction;
        SQL.Text := 'select * from ' + TableInfo.Fields[i].KeyTable;
        Open;
      end;
      LookupDataSource := TDataSource.Create(Self);
      LookupDataSource.DataSet := LookupQuery;
      rec[i].box := TDBLookupComboBox.Create(Self.Panel);
      with rec[i].box do begin
        Parent := Self.Panel;
        Top := i * 40;
        Width := 115;
        Left := 210;
        DataSource := Self.DataSource;
        DataField := fn;
        ListSource := LookupDataSource;
        KeyField := AFieldName;
        ListField := 'name';
        Style := csDropDownList;
      end;
    end;
  end;
  Show;
end;

function TEditForm.InsertQuery: String;
var
  data: string;
  i: integer;
begin
  for i := 1 to High(TableInfo.Fields) do
    data += Format(', :par%d', [i]);
  Result := Format('insert into %s values(next value for %s%s ) ',
    [TableInfo.TableName, TableInfo.Sequence, data]);
end;

function TEditForm.UpdateQuery: String;
var
  data, p: string;
  i: integer;
begin
  for i := 1 to High(TableInfo.Fields) do
    data += Format(', %s = :par%d', [TableInfo.Fields[i].FieldName, i]);
  Delete(data, 1, 1);
  Result := Format('update %s SET %s where id = %d',
    [TableInfo.TableName, data, Id]);
end;

procedure TEditForm.DeleteQuotes(var s: string);
begin
  if s[1] = '"' then s := Copy(s, 2, Length(s) - 2);
end;

procedure TEditForm.SetParams(AQuery: TSQLQuery);
var
  i: Integer;
  p: TParam;
begin
  for i := 1 to High(TableInfo.Fields) do begin
    p := AQuery.ParamByName('par' + IntToStr(i));
    if TableInfo.Fields[i].KeyField = '' then
      p.AsString := rec[i].Text.Text
    else
      p.AsInteger := SQLQuery.FieldByName(TableInfo.Fields[i].FieldName).AsInteger;
  end;
end;

procedure TEditForm.BtnSaveClick(Sender: TObject);
var
  s, p: string;
  i: integer;
  editQuery: TSQLQuery;
begin
  if Id = 0 then
    s := InsertQuery
  else
    s := UpdateQuery;
  editQuery := TSQLQuery.Create(nil);
  with editQuery do begin
    DataBase := ConnectForm.IBConnection;
    Transaction := ConnectForm.SQLTransaction;
    SQL.Text := s;
    SetParams(editQuery);
    ExecSQL;
    Free;
  end;
  FOnChange(self);
  Close;
end;

procedure TEditForm.DatasourceDataChange(Sender: TObject; Field: TField);
begin

end;

procedure TEditForm.BtnDelClick(Sender: TObject);
begin
  if MessageDlg('Вы действительно хотите отменить редактирование?',
    mtConfirmation, mbYesNo, 0) = mrYes
  then
    Close;
end;

end.

