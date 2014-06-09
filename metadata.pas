unit MetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Connect, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TFieldInfo }

  TFieldInfo = class
    FieldName, FieldCaption: string;
    Width: integer;
    KeyTable, KeyField: string;
    constructor Create(AFieldName, AFieldCaption: string; AWidth: integer;
      AKeyTable: string = ''; AKeyField: string = '');
  end;

  { TTableInfo }

  TTableInfo = class
    TableName, TableCaption: string;
    Fields: array of TFieldInfo;
    Sequence: string;
    constructor Create(
      ATableName, ATableCaption: string;
      AFields: array of TFieldInfo; ASequence: string);
    function MakeQuery(AIncludeRefs: Boolean = false): String;
  end;

var
  Meta: array of TTableInfo;

procedure AddTable(ATableInfo: TTableInfo);
procedure AddTable(ATableName, ATableCaption: string;
  AFields: array of TFieldInfo; ASequence: string);

implementation

uses References;

{ TTableInfo }

constructor TTableInfo.Create(ATableName, ATableCaption: string;
  AFields: array of TFieldInfo; ASequence: string);
var
  i: integer;
begin
  Sequence := ASequence;
  TableName := ATableName;
  TableCaption := ATableCaption;
  SetLength(Fields, Length(AFields));
  for i := 0 to High(AFields) do
    Fields[i] := AFields[i];
end;

{ TFieldInfo }

constructor TFieldInfo.Create(AfieldName, AFieldCaption: string;
  AWidth: integer; AKeyTable, AKeyField: string);
begin
  FieldName := AFieldName;
  FieldCaption := AFieldCaption;
  Width := AWidth;
  KeyTable := AKeyTable;
  KeyField := AKeyField;
end;

function TTableInfo.MakeQuery(AIncludeRefs: Boolean): String;
var
  TIField, TIQuery: string;
  i: integer;
begin
  TIField := '';
  TIQuery := '';
  for i := 0 to High(Fields) do begin
    if Fields[i].KeyTable <> '' then begin
      TIField += Format(', %s.Name', [Fields[i].KeyTable]);
      if AIncludeRefs then TIField += ', ' + Fields[i].FieldName;
      TIQuery += Format('INNER JOIN %s ON %s.%s = %s.%s ', [Fields[i].KeyTable,
        TableName, Fields[i].FieldName, Fields[i].KeyTable, Fields[i].KeyField]);
    end
    else
    if i <> 0 then
      TIField := Format('%s,%s', [TIField, Fields[i].FieldName])
    else
      TIField := Format('%s,%s.%s', [TIField, TableName, Fields[i].FieldName]);
  end;
  Delete(TIField, 1, 1);
  Result := Format('SELECT %s FROM %s %s', [TIField, TableName, TIQuery]);
end;

procedure AddTable(ATableInfo: TTableInfo);
begin
  SetLength(Meta, Length(Meta) + 1);
  Meta[High(Meta)] := ATableInfo;
end;

procedure AddTable(ATableName, ATableCaption: string; AFields: array of TFieldInfo;
  ASequence: string);
begin
  AddTable(TTableInfo.Create(ATableName, ATableCaption, AFields, ASequence));
end;


initialization

  AddTable('Subjects', 'Предметы', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('name', 'Название', 30)],
    'Subject_ID');
  AddTable('Subject_Types', 'Виды занятий', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('name', 'Вид', 10)],
    'Type_ID');
  AddTable('Professors', 'Преподаватели', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('name', 'ФИО', 15)],
    'Professor_ID');
  AddTable('Times', 'Пары', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('Name', '"Index"', 2),
    TFieldInfo.Create('"Begin"', 'Начало', 10),
    TFieldInfo.Create('"End"', 'Окончание', 5)],
    'Time_Index');
  AddTable('Days', 'Дни', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('name', 'День', 15)],
    'Day_Index');
  AddTable('Groups', 'Группы', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('name', 'Номер', 7),
    TFieldInfo.Create('Group_Size', 'Размер', 5)],
    'Group_ID');
  AddTable('Rooms', 'Аудитории', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('name', 'Номер', 15),
    TFieldInfo.Create('"Size"', 'Вместимость', 10)],
    'Room_ID');
  AddTable('Professors_Subjects', 'Преподаватели - предметы', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('Professor_ID', 'Преподаватель', 15, 'Professors', 'ID'),
    TFieldInfo.Create('Subject_ID', 'Предмет', 15, 'Subjects', 'ID')],
    'PS_ID');
  AddTable('Subjects_Groups', 'Предметы - группы', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('Subject_ID', 'Предмет', 15, 'Subjects', 'ID'),
    TFieldInfo.Create('Group_ID', 'Группа', 5, 'Groups', 'ID')],
    'SG_ID');
  AddTable('Schedule_Items', 'Расписание', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('Subject_ID', 'Предмет', 20, 'Subjects', 'ID'),
    TFieldInfo.Create('Subject_Type_ID', 'Вид занятия', 3, 'Subject_Types', 'ID'),
    TFieldInfo.Create('Professor_ID', 'Преподаватель', 10, 'Professors', 'ID'),
    TFieldInfo.Create('Time_Index', 'Пара', 3),
    TFieldInfo.Create('Day_Index', 'День недели', 7, 'Days', 'ID'),
    TFieldInfo.Create('Group_ID', 'Группа', 6, 'Groups', 'ID'),
    TFieldInfo.Create('Room_ID', 'Аудитория', 5, 'Rooms', 'ID'),
    TFieldInfo.Create('Week', 'Неделя (чет/нечет)', 3)],
    'Item_ID');
  AddTable('Schedule_Items', 'Вывести расписание', [
    TFieldInfo.Create('id', 'id', 2),
    TFieldInfo.Create('Subject_ID', 'Предмет', 20, 'Subjects', 'ID'),
    TFieldInfo.Create('Subject_Type_ID', 'Вид занятия', 3, 'Subject_Types', 'ID'),
    TFieldInfo.Create('Professor_ID', 'Преподаватель', 10, 'Professors', 'ID'),
    TFieldInfo.Create('Time_Index', '№ пары', 3, 'Times', 'ID'),
    TFieldInfo.Create('Day_Index', 'День недели', 7, 'Days', 'ID'),
    TFieldInfo.Create('Group_ID', 'Группа', 6, 'Groups', 'ID'),
    TFieldInfo.Create('Room_ID', 'Аудитория', 5, 'Rooms', 'ID'),
    TFieldInfo.Create('Week', 'Неделя', 3)],
    'Item_ID');
end.
