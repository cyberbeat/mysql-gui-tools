  T%structname% = class(TPersistent)
  private
    record_pointer: P%structname%;

%class_variables%
  public
%class_object%
%class_nested_structs%
%class_auxlists%
    constructor Create(%variable_list%); overload;
    constructor Create(record_pointer: P%structname%); overload;
    destructor Destroy; override;

    function get_record_pointer: P%structname%;

    procedure Assign(Source: TPersistent); override;

%class_properties%
  end;

  T%structname%_List = class(TObjectList)
  protected
    function GetItem(Index: Integer): T%structname%; overload;
    procedure SetItem(Index: Integer; AObject: T%structname%); overload;
  public
    function Add(AObject: T%structname%): Integer; overload;
    function Extract(Item: T%structname%): T%structname%; overload;
    function Remove(AObject: T%structname%): Integer; overload;
    function IndexOf(AObject: T%structname%): Integer; overload;
    procedure Insert(Index: Integer; AObject: T%structname%); overload;
    function First: T%structname%; overload;
    function Last: T%structname%; overload;
    
    property Items[Index: Integer]: T%structname% read GetItem write SetItem; default;
  end;
