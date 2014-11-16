unit AuxAdminBackupRestore;

interface

uses
  gnugettext, ComCtrls, Classes, SysUtils,
  myx_public_interface, myx_admin_public_interface,
  AuxFuncs, SchemataTreeView, ApplicationDataModule,
  MyxError, TntClasses, TntComCtrls, IniFiles, VirtualTrees;

const
  // Attention! These constants must be adjusted manually if they change in myx_admin_public_interface.h
  MYX_BTF_IGNORE_CONTENT  = $80000000;  // don't backup table content
  MYX_BTF_IS_TABLE        = $00000001;  // is a table
  MYX_BTF_IS_FUNCTION     = $00000002;  // is a stored function
  MYX_BTF_IS_PROCEDURE    = $00000004;  // is a stored procedure
  MYX_BTF_IS_VIEW         = $00000008;  // is a view
  MYX_BTF_IS_TRIGGER      = $00000010;  // is a trigger

type
  // Definition of BackupObj Types and Nodes
  TBackupObjType = (
    botCatalog,
    botSchema,
    botTable
  );

  TBackupCheckType = (
    bctNone,
    bctSome,
    bctAll
  );

  TBackupNode = class(TObject)
  private
    function GetEntry(Index: Integer): WideString;
    procedure SetEntry(Index: Integer; const Value: WideString);
    procedure SetSelected(const Value: TBackupCheckType);
  protected
    FObjName: WideString;
    FObjType: TBackupObjType;
    FSelected: TBackupCheckType;
    FEntries: array of WideString;
    FFlags: Integer;
  public
    constructor Create(ObjName: WideString; ObjType: TBackupObjType; Flags: integer);

    property Entries[Index: Integer]: WideString read GetEntry write SetEntry; default;
    property Flags: Integer read FFlags write FFlags;
    property ObjectName: WideString read FObjName write FObjName;
    property ObjectType: TBackupObjType read FObjType write FObjType;
    property SelectedState: TBackupCheckType read FSelected write SetSelected;
  end;

  PBackupNodeData = ^TBackupNodeData;
  TBackupNodeData = record
    BackupNode: TBackupNode;
    ImageIndex: Integer;
    Caption: WideString;
  end;

procedure BuildContentTreeFromBackupContent(PMySQL: Pointer; BackupTreeView: TVirtualStringTree;
  BackupContent: TMYX_BACKUP_CONTENT; SchemataFrame: TSchemataFrame; CompleteSchematas: Boolean);
procedure AddSchemaToBackup(PMySQL: Pointer; BackupTreeView: TVirtualStringTree; Schema: TMYX_SCHEMA;
  SelectAllNodes: Boolean);
function GetBackupContent(BackupTreeView: TVirtualStringTree): TMYX_BACKUP_CONTENT;
procedure BuildContentTreeFromRestoreContent(BackupTreeView: TVirtualStringTree; BackupContent: TMYX_BACKUP_CONTENT);
procedure SetNodeSelectState(Tree: TVirtualStringTree; Node: PVirtualNode; BackupCheckType: TBackupCheckType);
function AllChildNodeSelected(Tree: TVirtualStringTree; Node: PVirtualNode): TBackupCheckType;

//----------------------------------------------------------------------------------------------------------------------

implementation

const
  BackupCheckTypeToCheckState: array[TBackupCheckType] of TCheckState = (
    csUncheckedNormal, csMixedNormal, csCheckedNormal);


//----------------------------------------------------------------------------------------------------------------------

procedure BuildContentTreeFromBackupContent(PMySQL: Pointer; BackupTreeView: TVirtualStringTree;
  BackupContent: TMYX_BACKUP_CONTENT; SchemataFrame: TSchemataFrame; CompleteSchematas: Boolean);

  //----------------------------------------------------------------------------

  function IsNodeType(Node: PVirtualNode; AType: TBackupObjType): Boolean;

  // Checks if the given data has a valid backup object and if it is of the given
  // type. Returns True if so, otherwise False.

  var
    NodeData: PBackupNodeData;

  begin
    Result := False;
    if Assigned(Node) then
    begin
      NodeData := BackupTreeview.GetNodeData(Node);
      Result := Assigned(NodeData.BackupNode) and
        (NodeData.BackupNode.ObjectType = AType);
    end;
  end;

  //----------------------------------------------------------------------------

var
  I, J: integer;
  Schemas: TList;
  PCatalogs: PMYX_CATALOGS;
  Catalogs: TMYX_CATALOGS;
  Catalog: TMYX_CATALOG;
  Schema: TMYX_SCHEMA;
  Run: PVirtualNode;
  TableNode: PVirtualNode;
  SchemaNode: PVirtualNode;
  NodeData: PBackupNodeData;

begin
  BackupTreeView.Clear;

  if Assigned(SchemataFrame) then
    Catalogs := SchemataFrame.CatalogList
  else
  begin
    PCatalogs := myx_get_catalogs(PMySQL);
    if (PCatalogs = nil) then
      raise EMyxSQLError.Create('Error while fetching Catalog information', myx_mysql_errno(PMySQL),
        myx_mysql_error(PMySQL));
    try
      Catalogs := TMYX_CATALOGS.create(PCatalogs);
    finally
      myx_free_catalogs(PCatalogs);
    end;
  end;

  try
    // Add all schemas.
    Schemas := TList.Create;
    try
      Catalog := nil;
      Schema := nil;
      for I := 0 to BackupContent.tables.Count - 1 do
      begin
        if Assigned(Catalog) and Assigned(Schema)then
        begin
          if SameText(BackupContent.tables[I].catalog, Catalog.catalog_name) and
            SameText(BackupContent.tables[I].schema, Schema.schema_name) then
          begin
            Continue;
          end;
        end;

        // Find Catalog
        Catalog := nil;
        if Assigned(Catalogs) then
        begin
          for J := 0 to Catalogs.catalogs.Count - 1 do
            if SameText(Catalogs.catalogs[J].catalog_name, BackupContent.tables[I].catalog) or
              (SameText(BackupContent.tables[I].catalog, 'DEFAULT_CATALOG') and SameText(Catalogs.catalogs[J].catalog_name, 'def')) then
            begin
              Catalog := Catalogs.catalogs[J];
              Break;
            end;
        end;

        if Assigned(Catalog) then
        begin
          // Find Schema
          Schema:=nil;
          for J:=0 to Catalog.schemata.Count-1 do
            if SameText(Catalog.schemata[J].schema_name, BackupContent.tables[I].schema) then
            begin
              Schema:=Catalog.schemata[J];
              Break;
            end;

          // Add Schema if found and select it when CompleteSchematas is selected
          if Assigned(Schema) then
            AddSchemaToBackup(PMySQL, BackupTreeView, Schema, CompleteSchematas);
        end;
      end;
    finally
      Schemas.Free;
    end;
  finally
    if SchemataFrame = nil then
      Catalogs.Free;
  end;

  if not CompleteSchematas then
    with BackupTreeView do
    begin
      // Select tables stored in profile.
      Run := GetFirst;
      while Assigned(Run) do
      begin
        if IsNodeType(Run, botTable) then
        begin
          TableNode := Run;

          SchemaNode := nil;
          if IsNodeType(TableNode.Parent, botSchema) then
            SchemaNode := TableNode.Parent;

          if Assigned(SchemaNode) then
          begin
            for J := 0 to BackupContent.tables.Count - 1 do
            begin
              NodeData := GetNodeData(TableNode);
              if SameText(BackupContent.tables[J].table, NodeData.BackupNode.ObjectName) then
              begin
                NodeData := GetNodeData(SchemaNode);
                if SameText(BackupContent.tables[J].schema, NodeData.BackupNode.ObjectName) then
                begin
                  SetNodeSelectState(BackupTreeview, TableNode, bctAll);
                  Break;
                end;
              end;
            end;
          end;
        end
        else
        begin
          NodeData := GetNodeData(Run);
          if NodeData.BackupNode.ObjectType = botSchema then
          begin
            if GetFirstChild(Run) = nil then
              SetNodeSelectState(BackupTreeview, Run, bctAll);
          end;
        end;
        Run := GetNext(Run);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure BuildContentTreeFromRestoreContent(BackupTreeView: TVirtualStringTree; BackupContent: TMYX_BACKUP_CONTENT);

var
  I, Position: Integer;
  Catalogs: TTntStringList;
  Schematas: TTntStringList;
  SchemaNode: PVirtualNode;
  TableNode: PVirtualNode;
  BackupNode: TBackupNode;
  NodeData: PBackupNodeData;

begin
  BackupTreeView.Clear;

  with BackupTreeView do
  begin
    BeginUpdate;
    Catalogs := TTntStringList.Create;
    Schematas := TTntStringList.Create;
    try
      for I := 0 to BackupContent.tables.Count - 1 do
      begin
        Position := Schematas.IndexOf(BackupContent.tables[I].schema);
        SchemaNode := nil;
        if Position = -1 then
        begin
          SchemaNode := AddChild(nil);
          NodeData := GetNodeData(SchemaNode);
          NodeData.BackupNode := TBackupNode.Create(BackupContent.tables[I].catalog, botSchema, 0);
          NodeData.ImageIndex := 9;
          NodeData.Caption := BackupContent.tables[I].schema;
          CheckState[SchemaNode] := csCheckedNormal;
          Schematas.AddObject(NodeData.Caption, TObject(SchemaNode));
        end
        else
        begin
          if Assigned(Schematas.Objects[Position]) then
            SchemaNode := PVirtualNode(Schematas.Objects[Position]);
        end;

        if Assigned(SchemaNode) then
        begin
          BackupNode := TBackupNode.Create(BackupContent.tables[I].table, botTable, BackupContent.tables[I].flags);
          TableNode := AddChild(SchemaNode);
          NodeData := GetNodeData(TableNode);
          NodeData.BackupNode := BackupNode;
          NodeData.Caption := BackupContent.tables[I].table;

          case BackupContent.tables[I].flags and not MYX_BTF_IGNORE_CONTENT of
            MYX_BTF_IS_TABLE:
              begin
                NodeData.ImageIndex := 10;
                NodeData.BackupNode[0] := _('Table');
              end;
            MYX_BTF_IS_VIEW:
              begin
                NodeData.ImageIndex := 27;
                NodeData.BackupNode[0] := _('View');
              end;
            MYX_BTF_IS_PROCEDURE:
              begin
                NodeData.ImageIndex := 13;
                NodeData.BackupNode[0] := _('Stored procedure');
              end;
            MYX_BTF_IS_FUNCTION:
              begin
                NodeData.ImageIndex := 13;
                NodeData.BackupNode[0] := _('Stored function');
              end;
            MYX_BTF_IS_TRIGGER:
              begin
                NodeData.ImageIndex := 10;
                NodeData.BackupNode[0] := _('Trigger');
              end;
          end;
        end;
      end;

      for I := 0 to Catalogs.Count-1 do
        if Assigned(Catalogs.Objects[I]) then
        begin
          SetNodeSelectState(BackupTreeview, PVirtualNode(Catalogs.Objects[I]), bctAll);
          Expanded[PVirtualNode(Catalogs.Objects[I])] := True;
        end;
    finally
      EndUpdate;
      Schematas.Free;
      Catalogs.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddSchemaToBackup(PMySQL: Pointer; BackupTreeView: TVirtualStringTree; Schema: TMYX_SCHEMA;
  SelectAllNodes: Boolean);

var
  SchemaNode: PVirtualNode;
  TableNode: PVirtualNode;
  NodeData: PBackupNodeData;
  I: Integer;
  RawEntityStatus: PMYX_SCHEMA_ENTITY_STATUS;
  EntityStatus: TMYX_SCHEMA_ENTITY_STATUS;
  RawTableStatus: PMYX_TABLE_STATUS;
  TableStatus: TMYX_TABLE_STATUS;
  RawViewStatus: PMYX_VIEW_STATUS;
  ViewStatus: TMYX_VIEW_STATUS;
  RawPSPStatus: PMYX_SCHEMA_STORED_PROCEDURE;
  PSPStatus: TMYX_SCHEMA_STORED_PROCEDURE;
  BackupNode: TBackupNode;

begin
  with BackupTreeview do
  begin
    BeginUpdate;
    try
      SchemaNode := AddChild(nil);
      NodeData := GetNodeData(SchemaNode);
      NodeData.BackupNode := TBackupNode.Create(Schema.schema_name, botSchema, 0);
      NodeData.ImageIndex := 9;
      NodeData.Caption := Schema.schema_name;

      // Get Entities
      RawEntityStatus := myx_get_schema_entity_status(PMySQL, '', Schema.schema_name);
      try
        EntityStatus := TMYX_SCHEMA_ENTITY_STATUS.Create(RawEntityStatus);

        for I := 0 to EntityStatus.schema_entities.Count - 1 do
        begin
          case EntityStatus.schema_entities[I].entity_type of
            MYX_ENTITY_TABLE:
              begin
                RawTableStatus := EntityStatus.schema_entities[I].entity;
                TableStatus := TMYX_TABLE_STATUS.Create(RawTableStatus);
                BackupNode := TBackupNode.Create(TableStatus.table_name, botTable, 0);
                BackupNode[0] := TableStatus.table_type;
                BackupNode[1] := TableStatus.rows;
                BackupNode[2] := TableStatus.data_length;
                BackupNode[3] := TableStatus.update_time;
                if SelectAllNodes then
                  BackupNode.SelectedState := bctAll;

                TableNode := AddChild(SchemaNode);
                NodeData := GetNodeData(TableNode);
                NodeData.BackupNode := BackupNode;
                NodeData.ImageIndex := 10;
                NodeData.Caption := TableStatus.table_name;
              end;
            MYX_ENTITY_VIEW:
              begin
                RawViewStatus := EntityStatus.schema_entities[I].entity;
                ViewStatus := TMYX_VIEW_STATUS.Create(RawViewStatus);
                BackupNode := TBackupNode.Create(ViewStatus.view_name, botTable, 0);
                BackupNode[0] := 'VIEW';
                if SelectAllNodes then
                  BackupNode.SelectedState := bctAll;

                TableNode := AddChild(SchemaNode);
                NodeData := GetNodeData(TableNode);
                NodeData.BackupNode := BackupNode;
                NodeData.ImageIndex := 27;
                NodeData.Caption := ViewStatus.view_name;
              end;
            MYX_ENTITY_PROC:
              begin
                RawPSPStatus := EntityStatus.schema_entities[I].entity;
                PSPStatus := TMYX_SCHEMA_STORED_PROCEDURE.Create(RawPSPStatus);
                BackupNode := TBackupNode.Create(PSPStatus.name, botTable, 0);
                BackupNode[0] := 'PROCEDURE';
                if SelectAllNodes then
                  BackupNode.SelectedState := bctAll;

                TableNode := AddChild(SchemaNode);
                NodeData := GetNodeData(TableNode);
                NodeData.BackupNode := BackupNode;
                NodeData.ImageIndex := 13;
                NodeData.Caption := PSPStatus.name;
              end;
            MYX_ENTITY_FUNC:
              begin
                RawPSPStatus := EntityStatus.schema_entities[I].entity;
                PSPStatus := TMYX_SCHEMA_STORED_PROCEDURE.Create(RawPSPStatus);
                BackupNode :=
                  TBackupNode.Create(PSPStatus.name, botTable, 0);
                BackupNode[0] := 'FUNCTION';
                if SelectAllNodes then
                  BackupNode.SelectedState := bctAll;

                TableNode := AddChild(SchemaNode);
                NodeData := GetNodeData(TableNode);
                NodeData.BackupNode := BackupNode;
                NodeData.ImageIndex := 13;
                NodeData.Caption := PSPStatus.name;
              end;
          end;
        end;

        // Validate all new nodes, so they get their proper check box etc.
        ValidateNode(SchemaNode, True);

        // Select new schema and all subnodes (tables) in the treeview.
        if SelectAllNodes then
          SetNodeSelectState(BackupTreeview, SchemaNode, bctAll);

        EntityStatus.Free;
      finally
        myx_free_schema_entity_status(RawEntityStatus);
      end;

      // Expand nodes if there is only one schema.
      if RootNodeCount = 1 then
        Expanded[GetFirst] := True;
    finally
      EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetBackupContent(BackupTreeView: TVirtualStringTree): TMYX_BACKUP_CONTENT;

var
  Table: TMYX_BACKUP_TABLE;
  Run: PVirtualNode;
  TableNodeData: PBackupNodeData;
  SchemaNodeData: PBackupNodeData;
  TableNode: PVirtualNode;
  SchemaNode: PVirtualNode;
  SchemaName: WideString;
  Flags: Integer;

begin
  Result := TMYX_BACKUP_CONTENT.create;
  try
    with BackupTreeview do
    begin
      Run := GetFirst;
      while Assigned(Run) do
      begin
        TableNodeData := GetNodeData(Run);
        if (TableNodeData.BackupNode.ObjectType = botTable) and (TableNodeData.BackupNode.SelectedState = bctAll) then
        begin
          TableNode := Run;
          SchemaName := '';

          SchemaNode := TableNode.Parent;
          SchemaNodeData := GetNodeData(SchemaNode);

          if SchemaNodeData.BackupNode.ObjectType = botSchema then
            SchemaName := SchemaNodeData.Caption;

          if TableNodeData.BackupNode[0] <> '' then
          begin
            if TableNodeData.BackupNode[0] = 'TRIGGER' then
              Flags := MYX_BTF_IS_TRIGGER
            else
              if TableNodeData.BackupNode[0] = 'VIEW' then
                Flags := MYX_BTF_IS_VIEW
              else
                if TableNodeData.BackupNode[0] = 'PROCEDURE' then
                  Flags := MYX_BTF_IS_PROCEDURE
                else
                  if TableNodeData.BackupNode[0] = 'FUNCTION' then
                    Flags := MYX_BTF_IS_FUNCTION
                  else
                    Flags := MYX_BTF_IS_TABLE;
          end
          else
            Flags := TableNodeData.BackupNode.Flags;

          Table := TMYX_BACKUP_TABLE.create('def', SchemaName, TableNodeData.Caption, Flags);
          Result.tables.Add(Table);
        end
        else
          if (TableNodeData.BackupNode.ObjectType = botSchema) and
            (TableNodeData.BackupNode.SelectedState = bctAll) and
            (GetFirstChild(Run) = nil) then
          begin
            SchemaName := TableNodeData.Caption;
            Table := TMYX_BACKUP_TABLE.create('def', SchemaName, '', 0);
            Result.tables.Add(Table);
          end;

        Run := GetNext(Run);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetNodeSelectState(Tree: TVirtualStringTree; Node: PVirtualNode; BackupCheckType: TBackupCheckType);

var
  Data: PBackupNodeData;

begin
  with Tree do
  begin
    Data := GetNodeData(Node);
    if Data.BackupNode.SelectedState <> BackupCheckType then
    begin
      Data.BackupNode.SelectedState := BackupCheckType;
      CheckState[Node] := BackupCheckTypeToCheckState[BackupCheckType];
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function AllChildNodeSelected(Tree: TVirtualStringTree; Node: PVirtualNode): TBackupCheckType;

var
  ChildNode: PVirtualNode;
  NodeData: PBackupNodeData;
  AllSelected: Boolean;
  AtLeastOneSelected: Boolean;
  SubChildsSelectState: TBackupCheckType;

begin
  AllSelected := True;
  AtLeastOneSelected := False;

  with Tree do
  begin
    // Check if all child nodes are selected.
    ChildNode := GetFirstChild(Node);
    while Assigned(ChildNode) do
    begin
      NodeData := GetNodeData(ChildNode);
      AllSelected := AllSelected and (NodeData.BackupNode.SelectedState = bctAll);

      AtLeastOneSelected := AtLeastOneSelected or (NodeData.BackupNode.SelectedState in [bctAll, bctSome]);

      // Call recursive if the child has another child.
      if Assigned(ChildNode.FirstChild) then
      begin
        SubChildsSelectState := AllChildNodeSelected(Tree, ChildNode);
        AllSelected := AllSelected and (SubChildsSelectState = bctAll);
        AtLeastOneSelected := AtLeastOneSelected or (SubChildsSelectState in [bctAll, bctSome]);
      end;

      ChildNode := GetNextSibling(ChildNode);
    end;

    if AllSelected then
      Result := bctAll
    else
      if AtLeastOneSelected then
        Result := bctSome
      else
        Result :=bctNone;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TBackupNode.Create(ObjName: WideString; ObjType: TBackupObjType; Flags: Integer);

begin
  FObjName := ObjName;
  FObjType := ObjType;
  FSelected := bctNone;
  FFlags := Flags;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBackupNode.GetEntry(Index: Integer): WideString;

begin
  if Index < Length(FEntries) then
    Result := FEntries[Index]
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBackupNode.SetEntry(Index: Integer; const Value: WideString);

begin
  if Index >= Length(FEntries) then
    SetLength(FEntries, Index + 1);
  FEntries[Index] := Value
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBackupNode.SetSelected(const Value: TBackupCheckType);

begin
  FSelected := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
