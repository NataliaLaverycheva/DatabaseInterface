unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  MetaData, Connect, References, StdCtrls, TimeTable;

type

  { TSchedule }

  TSchedule = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuExit: TMenuItem;
    MenuReferences: TMenuItem;
    MenuHelps: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuHelpsClick(Sender: TObject);
    procedure TableShow(Sender: TObject);
    procedure ShowSchedule(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Schedule: TSchedule;

implementation

{$R *.lfm}

{ TSchedule }

procedure TSchedule.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSchedule.FormCreate(Sender: TObject);
var i: integer;
  M: array of TMenuItem;
begin
  for i := 0 to High(Meta) do begin
    SetLength(M, Length(M) + 1);
    M[High(M)] := TMenuItem.Create(nil);
    with M[High(M)] do begin
      Caption := Meta[i].TableCaption;
      Name := 'References' + Meta[i].TableName;
      Tag := i;
      OnClick := @TableShow;
    end;
  end;
  M[High(Meta)].OnClick := @ShowSchedule;
  MenuReferences.Add(M);
end;

procedure TSchedule.TableShow(Sender: TObject);
var
  FTag: integer;
begin
  SetLength(FReferences, Length(FReferences) + 1);
  FTag := (Sender as TMenuItem).Tag;
  FReferences[FTag] := TReferences.Create(nil);
  with FReferences[FTag] do begin
    Table := Meta[FTag];
    Id := FTag;
    Tag := FTag;
    ShowTable(Meta[FTag]);
  end;
end;

procedure TSchedule.ShowSchedule(Sender: TObject);
var
  i: integer;
begin
  i := (Sender as TMenuItem).Tag;
  T := TT.Create(nil);
  T.Table := Meta[i];
  T.ShowTimeTable;
end;

procedure TSchedule.MenuHelpsClick(Sender: TObject);
begin
  ShowMessage('Б8103а, Лавёрычева Наталья');
end;

end.

