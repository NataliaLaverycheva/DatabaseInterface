unit Conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Connect, sqldb, DB;

type

  TConflictId = record
    Id1, Id2: integer;
  end;

  { TConflictForm }

  TConflictForm = class(TForm)
    SQLQuery: TSQLQuery;
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
  private
    ConflictId: array of array of TConflictId;
    procedure GetDataConflict;
    { private declarations }
  public
    ConflictsName: array of string;
    procedure GetTree;
    { public declarations }
  end;

var
  ConflictQuery: array of string;
  procedure AddConflictQuery(AQuery: string);

implementation

{$R *.lfm}

procedure AddConflictQuery(AQuery: string);
begin
  SetLength(ConflictQuery, Length(ConflictQuery) + 1);
  ConflictQuery[High(ConflictQuery)] := AQuery;
end;

procedure TConflictForm.FormCreate(Sender: TObject);
begin
  SQLQuery := TSQLQuery.Create(Self);
  SQLQuery.DataBase := ConnectForm.IBConnection;
  SQLQuery.Transaction := ConnectForm.SQLTransaction;

  SetLength(ConflictsName, Length(ConflictQuery));
  ConflictsName[0] := 'Разные пары в одной аудитории';
  ConflictsName[1] := 'Преподаватель в разных аудиториях';
  ConflictsName[2] := 'Преподаватель на разных парах';
  ConflictsName[3] := 'Группа в разных аудиториях';
  ConflictsName[4] := 'Группа на разных парах';
  ConflictsName[5] := 'Дублирующиеся пары';
end;

procedure TConflictForm.GetDataConflict;
var
  i: integer;
  ConfId: TConflictId;
begin
  SetLength(ConflictId, Length(ConflictQuery));
  for i := 0 to High(ConflictId) do begin
    with SQLQuery do begin
      Close;
      SQL.Text := ConflictQuery[i];
      Open;
      First;
       while not EOF do begin
         SetLength(ConflictId[i], Length(ConflictId[i]) + 1);
         ConfId.Id1 := SQLQuery.Fields[0].AsInteger;
         ConfId.Id2 := SQLQuery.Fields[9].AsInteger;
         ConflictId[i, High(ConflictId[i])] := ConfId;
         Next;
       end;
    end;
  end;
end;

procedure TConflictForm.GetTree;
var
  i, j: integer;
  MainTreeNode, ConfTreeNode, ChildConfTreeNode: TTreeNode;
begin
  GetDataConflict;
  MainTreeNode := TreeView.Items.Add(nil, 'Конфликты');
  for i := 0 to High(ConflictQuery) do begin
    ConfTreeNode := TreeView.Items.AddChild(MainTreeNode,
      Format('%d. %s', [i + 1, ConflictsName[i]]));
      for j := 0 to High(ConflictId[i]) do begin
        ChildConfTreeNode := TreeView.Items.AddChild(
          ConfTreeNode, Format('Конфликт между %d и %d',
          [ConflictId[i, j].Id1, ConflictId[i, j].Id2]));
      end;
  end;
end;

{ TConflictForm }
initialization

  AddConflictQuery('SELECT S1.*, S2.* FROM SCHEDULE_ITEMS S1 INNER JOIN SCHEDULE_ITEMS S2 ON S1.TIME_INDEX=S2.TIME_INDEX AND S1.DAY_INDEX=S2.DAY_INDEX AND s1.ROOM_ID=S2.ROOM_ID AND  S1.PROFESSOR_ID<>S2.PROFESSOR_ID AND S1.ID<S2.ID');
  AddConflictQuery('SELECT s1.*, s2.* FROM SCHEDULE_ITEMS S1 INNER JOIN SCHEDULE_ITEMS S2 ON S1.PROFESSOR_ID=S2.PROFESSOR_ID AND S1.TIME_INDEX=S2.TIME_INDEX AND S1.DAY_INDEX=S2.DAY_INDEX AND S1.ROOM_ID<>S2.ROOM_ID AND S1.ID<S2.ID');
  AddConflictQuery('SELECT S1.*, S2.* FROM SCHEDULE_ITEMS S1 INNER JOIN SCHEDULE_ITEMS S2 ON S1.PROFESSOR_ID=S2.PROFESSOR_ID AND S1.TIME_INDEX=S2.TIME_INDEX AND S1.DAY_INDEX=S2.DAY_INDEX AND S1.SUBJECT_ID<>S2.SUBJECT_ID AND S1.ID<S2.ID');
  AddConflictQuery('SELECT S1.*, S2.* FROM SCHEDULE_ITEMS S1 INNER JOIN SCHEDULE_ITEMS S2 ON S1.TIME_INDEX=S2.TIME_INDEX AND S1.DAY_INDEX=S2.DAY_INDEX AND S1.GROUP_ID=S2.GROUP_ID AND S1.ROOM_ID<>S2.ROOM_ID AND S1.ID<S2.ID');
  AddConflictQuery('SELECT S1.*, S2.* FROM SCHEDULE_ITEMS S1 INNER JOIN SCHEDULE_ITEMS S2 ON S1.TIME_INDEX=S2.TIME_INDEX AND S1.DAY_INDEX=S2.DAY_INDEX AND S1.GROUP_ID=S2.GROUP_ID AND S1.SUBJECT_ID<>S2.SUBJECT_ID AND S1.ID<S2.ID');
  AddConflictQuery('SELECT S1.*, S2.* FROM SCHEDULE_ITEMS S1 INNER JOIN SCHEDULE_ITEMS S2 ON S1.TIME_INDEX=S2.TIME_INDEX AND S1.DAY_INDEX=S2.DAY_INDEX AND S1.GROUP_ID=S2.GROUP_ID AND S1.SUBJECT_ID=S2.SUBJECT_ID AND s1.SUBJECT_TYPE_ID=s2.SUBJECT_TYPE_ID AND s1.PROFESSOR_ID=s2.PROFESSOR_ID AND S1.ROOM_ID=S2.ROOM_ID AND S1.ID<S2.ID');

end.


