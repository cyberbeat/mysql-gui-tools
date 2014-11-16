unit EditorSql;

// Copyright (C) 2008 MySQL AB, 2008 Sun Microsystems, Inc.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, UniCodeEditor, UCEHighlighter,
  UCESQLHighlighter, StdCtrls, TntStdCtrls, MySQLConnection,
  AuxFuncs, TntForms, Options, myx_public_interface, AuxApplicationFuncs,
  gnugettext;

type
  EditorContentType = (StoredRoutine, View);

  TEditorSqlForm = class(TTntForm)
    BottomPnl: TTntPanel;
    ExecuteSQLBtn: TTntButton;
    CancelBtn: TTntButton;
    UCESQLHighlighter: TUCESQLHighlighter;
    SqlUCE: TUniCodeEdit;
    SepBevel: TTntBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ExecuteSQLBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SqlUCEKeyPress(Sender: TObject; var Key: Char);

  protected
    procedure SetSql(Sql: WideString);
    procedure SetPreSql(Sql: WideString);
    procedure SetSqlSchema(Sch: WideString);

  private
    FMySQLConn: TMySQLConn;
    FRefreshSchemaObjects: TNotifyEvent;
    FDropSQL: WideString;
    FSqlSchema: WideString;
    FOriginalName: WideString;  // currently used oly for views
    FOriginalSQL: WideString;
    ContentType: EditorContentType;
    FChanged: Boolean;
    function GetSql: WideString;
    function ApplyChanges: Boolean;
  public
    property SqlSchema: WideString read FSqlSchema write SetSqlSchema;
    property DropSQL: WideString read FDropSQL write SetPreSql;
    property Sql: WideString read GetSql write SetSql;
    property MySQLConn: TMySQLConn read FMySQLConn write FMySQLConn;
    property RefreshSchemaObjects: TNotifyEvent read FRefreshSchemaObjects write FRefreshSchemaObjects;
    procedure Show;
    procedure SetContentType(ct: EditorContentType);    
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  FMySQLConn := nil;
  FRefreshSchemaObjects := nil;
  FChanged := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.SetContentType(ct: EditorContentType);

begin
  ContentType := ct;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

var
  Result: Integer;

begin
  CanClose := True;
  if FChanged then
  begin
    Result := ShowModalDialog(_('Pending Changes'), _('There are unapplied changes. Do you want to apply them now?'),
      myx_mtConfirmation, _('Apply') + #13#10 + _('Discard')+ #13#10 + _('Cancel'));
    case Result of
      1:
        CanClose := ApplyChanges;
      2: // Ignore
        FChanged := False;
      3:
        CanClose := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.Show;

begin
  if(MYXCommonOptions.EditorKeepRoutineEditorOnTop)then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.SqlUCEKeyPress(Sender: TObject; var Key: Char);

begin
  FChanged := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEditorSqlForm.GetSql: WideString;

begin
  Result := SqlUCE.Content.Text;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.SetSql(Sql: WideString);

begin
  SqlUCE.Content.Text := Sql;
  FOriginalSQL := Sql;
  FOriginalName := myx_dbm_get_view_name_from_query(Sql);
  FChanged := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.SetSqlSchema(Sch: WideString);

begin
  FSqlSchema:=Sch;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.SetPreSql(Sql: WideString);

begin
  FDropSQL := Sql;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.ExecuteSQLBtnClick(Sender: TObject);

begin
  if ApplyChanges then
    Close;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEditorSqlForm.ApplyChanges: Boolean;

// Applies the current changes in the editor to the object and returns True if that was succesfull.

var
  NewObjectName: WideString;
  DropOriginalView: Boolean;
  OldDropSQL: WideString;

begin
  Result := not FChanged;

  if not FChanged then
    if ShowOptionalModalDialog(_('No changes to apply'),
      _('There were no changes. Do you still want to send the SQL command? '),
      myx_mtWarning, _('Yes') + #13#10 + _('No'), True, '') = 2 then
      Exit;

  DropOriginalView := False;
  OldDropSQL := FDropSQL;

  if Assigned(FMySQLConn) then
  begin
    if FSqlSchema <> '' then
      MySQLConn.DefaultSchema := SqlSchema;

    if FDropSQL <> '' then
    begin
      NewObjectName := myx_dbm_get_view_name_from_query(Sql);
      if (FOriginalName <> '') and (NewObjectName <> FOriginalName) then
      begin
        if ShowOptionalModalDialog(_('Object name has changed'),
          _('Would you like to keep the original object? '),
          myx_mtWarning, _('Yes') + #13#10 + _('No'), True, '') = 2 then
        begin
          DropOriginalView := True;
        end;
      end
      else
        if ContentType = View then
        begin
          // old view definition is not dropped silently anymore
          // instead we add 'or replace' to view's definition
          // when start editing
          //FMySQLConn.ExecuteDirect(PreSql);
          FDropSQL := '';
        end
        else
        begin
          // Assume editing a stored routine here.
          // Drop old definition, we have a copy for restauration if necessary.
          FMySQLConn.ExecuteDirect(DropSQL);
          FDropSQL := '';
        end
    end;

    if FMySQLConn.ExecuteDirect(Sql) then
    begin
      if DropOriginalView then
      begin
        FMySQLConn.ExecuteDirect(FDropSQL);
        FDropSQL := '';
      end;

      if (Assigned(FRefreshSchemaObjects)) then
        FRefreshSchemaObjects(Self);
        
      Result := True;
    end
    else
    begin
      // A problem appeared so we need to restore the original SQL unless it isn't dropped yet.
      if FDropSQL = '' then
      begin
        FMySQLConn.ExecuteDirect(FOriginalSQL);
        FDropSQL := OldDropSQL;
      end;
    end;
  end;

  if Result then
    FChanged := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorSqlForm.CancelBtnClick(Sender: TObject);

begin
  FChanged := False;
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
