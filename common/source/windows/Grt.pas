unit Grt;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

interface

uses
  gnugettext, TntSystem,
  Windows, SysUtils, Classes, TntClasses, Forms, TntSysUtils, Messages,
  myx_public_interface,
  myx_util_public_interface,
//  myx_sql_resultset_public_interface,
  myx_grt_public_interface,
  myx_grt_builtin_module_public_interface,
  UniCodeConsole, SyncObjs, AuxFuncs;

type
  TGrt = class;
  TGrtEngine = class;

  GrtLoaderState =
  (
    GrtLsNotInitialized,
    GrtLsInitialized,
    GrtLsInitializeFailed,
    GrtLsModulesLoaded
  );

  GrtValueType =
  (
    GrtAnyValue = Ord(MYX_ANY_VALUE),
    GrtIntValue = Ord(MYX_INT_VALUE),
    GrtRealValue = Ord(MYX_REAL_VALUE),
    GrtStringValue = Ord(MYX_STRING_VALUE),
    GrtListValue = Ord(MYX_LIST_VALUE),
    GrtDictValue = Ord(MYX_DICT_VALUE)
  );

  GrtValueCallbackReason =
  (
    GrtVcrDelete = Ord(MYX_GVCR_DELETE),
    GrtVcrDictItemChange = Ord(MYX_GVCR_DICT_ITEM_CHANGE)
  );

  IGrtGenericTask = interface;
  
  TGrtProcessOutput = procedure(Text: WideString) of object;
  TGrtProcessMessages = procedure(Messages: TMYX_GRT_MSGS) of object;
  TGrtPerformInput = function(Description: WideString): WideString of object;
  TGrtPerformStatusQuery = function: integer of object;
  TGrtOnShutDown = procedure(Grt: TGrt) of object;
  TGrtTaskFinished = procedure(Task: IGrtGenericTask) of object;

  IGrtValueCallbackListener = interface
    function ValueChange(Value: Pointer; Reason: GrtValueCallbackReason): Integer;
  end;

  IGrtModuleImplementor = interface
    function GrtExecuteModuleFunction(ModuleName: WideString; FunctionName: WideString; arguments: Pointer): Pointer;
  end;

  // Communication interfaces for tasks and their principals.
  IGrtGenericTask = interface
  ['{42C4FCC5-2E9D-4500-AEE7-DE17031B8E6F}']
    function GetAllowNullResult: Boolean;
    function GetDataObj: TObject;
    function GetErrorCode: MYX_GRT_ERROR;
    function GetErrorString: WideString;
    function GetExecutionTime: Cardinal;
    function GetProcessOutputFunction: TGrtProcessOutput;
    function GetTag: Integer;
    function GetTaskFinishedFunction: TGrtTaskFinished;
    function GetTaskID: Integer;
    function GetTimeout: Integer;
    procedure PrepareSynchronization;
    procedure SetErrorCode(const Value: MYX_GRT_ERROR);
    procedure SetErrorString(const Value: WideString);
    procedure SetEvent;
    procedure SetExecutionTime(const Value: Cardinal);
    procedure SetTag(Tag: Integer);
    procedure WaitFor(WithMessages: Boolean);

    property AllowNullResult: Boolean read GetAllowNullResult;
    property DataObj: TObject read GetDataObj;
    property ErrorCode: MYX_GRT_ERROR read GetErrorCode write SetErrorCode;
    property ErrorString: WideString read GetErrorString write SetErrorString;
    property ExecutionTime: Cardinal read GetExecutionTime write SetExecutionTime;
    property TaskID: Integer read GetTaskID;
    property Timeout: Integer read GetTimeout;
    property Tag: Integer read GetTag write SetTag;

    property OnProcessOutput: TGrtProcessOutput read GetProcessOutputFunction;
    property OnTaskFinished: TGrtTaskFinished read GetTaskFinishedFunction;
  end;

  IGrtTask = interface(IGrtGenericTask)
  ['{A995FC94-DE83-4977-B23C-CAC6C9401CA9}']
    function GetDescription: WideString;
    function GetFunctionArgument: Pointer;
    function GetFunctionName: WideString;
    function GetModulName: WideString;
    function GetPerformInputFunction: TGrtPerformInput;
    function GetPerformStatusQueryFunction: TGrtPerformStatusQuery;
    function GetProcessMessagesFunction: TGrtProcessMessages;
    function GetResult: Pointer;
    function GetSearchParent: Boolean;
    procedure SetResult(const Value: Pointer);

    property Description: WideString read GetDescription;
    property FunctionArgument: Pointer read GetFunctionArgument;
    property FunctionName: WideString read GetFunctionName;
    property ModuleName: WideString read GetModulName;
    property Result: Pointer read GetResult write SetResult;
    property SearchParent: Boolean read GetSearchParent;

    property OnPerformInput: TGrtPerformInput read GetPerformInputFunction;
    property OnPerformStatusQuery: TGrtPerformStatusQuery read GetPerformStatusQueryFunction;
    property OnProcessMessages: TGrtProcessMessages read GetProcessMessagesFunction;
  end;

  IGrtShellTask = interface(IGrtGenericTask)
  ['{9FA380B3-8B4D-49F9-961B-E217B8AE82B9}']
    function GetCommand: WideString;
    function GetResult: MYX_GRT_SHELL_COMMAND;
    procedure SetResult(const Value: MYX_GRT_SHELL_COMMAND);

    property Command: WideString read GetCommand;
    property Result: MYX_GRT_SHELL_COMMAND read GetResult write SetResult;
  end;

  // The main Delphi wrapper class to the Generic Runtime library.
  TGrt = class
  private
    FAccessLock: TCriticalSection;
    FNextTaskID: Integer;
    FStructIconsList: TTntStringList;

    FConsole: TUniCodeConsole;

    FVerbose: Boolean;
    FRemoteDebug: Boolean;
    FJvmLibrary: WideString;
    FJvmMaxHeap: WideString;
    FJavaClasspath: WideString;
    FShellInterface: WideString;

    FOnShutDown: TGrtOnShutDown;
  protected
    GrtSyncEvent: TEvent;
    FGrtEngine: TGrtEngine;

    function CreateGrtEngine: TGrtEngine; virtual;

    function GetJavaLoaderState: GrtLoaderState;
    function GetLuaLoaderState: GrtLoaderState;
    function GetBuiltinLoaderState: GrtLoaderState;
    {$ifdef ENABLE_PHP_MODULES}
    function GetPhpLoaderState: GrtLoaderState;
    {$endif}
    function GetUiLoaderState: GrtLoaderState;

    procedure OutputCommandLine(Text: WideString);

    function GrtNativeGrt: Pointer;

    function GetGlobal(const Path: WideString): Pointer;
    procedure SetGlobal(const Path: WideString; NewValue: Pointer);
    function GetGlobalAsString(const Path: WideString): WideString;
    procedure SetGlobalAsString(const Path: WideString; NewValue: WideString);
    function GetGlobalAsInt(const Path: WideString): Integer;
    procedure SetGlobalAsInt(const Path: WideString; NewValue: Integer);
    function GetGlobalAsReal(const Path: WideString): Double;
    procedure SetGlobalAsReal(const Path: WideString; NewValue: Double);
    function GetGlobalRef(const Path: WideString): Pointer;
    procedure SetGlobalRef(const Path: WideString; NewValue: Pointer);

    function GetListItem(const List: Pointer; const I: Integer): Pointer;
    procedure SetListItem(const List: Pointer; const I: Integer; NewValue: Pointer);
    function GetListString(const List: Pointer; const I: Integer): WideString;
    procedure SetListString(const List: Pointer; const I: Integer; NewValue: WideString);
    function GetListRefItem(const List: Pointer; const I: Integer): Pointer;
    procedure SetListRefItem(const List: Pointer; const I: Integer; NewValue: Pointer);
    function GetListItemByObjectName(const List: Pointer; const Name: WideString): Pointer;
    function GetListRefValueByObjectName(const List: Pointer; const Name: WideString): Pointer;

    function GetDictItem(const Dict: Pointer; const Key: WideString): Pointer;
    procedure SetDictItem(const Dict: Pointer; const Key: WideString; NewValue: Pointer);
    function GetDictString(const Dict: Pointer; const Key: WideString): WideString;
    procedure SetDictString(const Dict: Pointer; const Key: WideString; NewValue: WideString);
    function GetDictInt(const Dict: Pointer; const Key: WideString): Integer;
    procedure SetDictInt(const Dict: Pointer; const Key: WideString; NewValue: Integer);
    function GetDictReal(const Dict: Pointer; const Key: WideString): Double;
    procedure SetDictReal(const Dict: Pointer; const Key: WideString; NewValue: Double);
    function GetDictRef(const Dict: Pointer; const Key: WideString): Pointer;
    procedure SetDictRef(const Dict: Pointer; const Key: WideString; NewValue: Pointer);
    function GetDictKey(const Dict: Pointer; const I: Integer): WideString;

    function GetDictStructName(const Dict: Pointer): WideString;
    procedure SetDictStructName(const Dict: Pointer; StructName: WideString);

    function GetValueInt(const Value: Pointer): Integer;
    function GetValueString(const Value: Pointer): WideString;

    function GetOutputBuffer: WideString;
    procedure SetOutputBuffer(OutputBuffer: WideString);

    function GetShellVar(Name: WideString): Pointer;
    procedure SetShellVar(Name: WideString; Value: Pointer);
  public
    constructor Create(Console: TUniCodeConsole);
    destructor Destroy; override;

    property NativeGrt: Pointer read GrtNativeGrt;
    property Console: TUniCodeConsole read FConsole write FConsole;

    property OnShutDown: TGrtOnShutDown read FOnShutDown write FOnShutDown;

    // Options
    property Verbose: Boolean read FVerbose write FVerbose;
    property RemoteDebug: Boolean read FRemoteDebug write FRemoteDebug;
    property JvmLibrary: WideString read FJvmLibrary write FJvmLibrary;
    property JvmMaxHeap: WideString read FJvmMaxHeap write FJvmMaxHeap;
    property ShellInterface: WideString read FShellInterface write FShellInterface;

    // Loaders
    property JavaLoaderState: GrtLoaderState read GetJavaLoaderState;
    property LuaLoaderState: GrtLoaderState read GetLuaLoaderState;
    property BuiltinLoaderState: GrtLoaderState read GetBuiltinLoaderState;
    {$ifdef ENABLE_PHP_MODULES}
    property PhpLoaderState: GrtLoaderState read GetPhpLoaderState;
    {$endif}
    property UiLoaderState: GrtLoaderState read GetUiLoaderState;

    // Access values
    property Global[const Path: WideString]: Pointer read GetGlobal write SetGlobal;
    property GlobalAsString[const Path: WideString]: WideString read GetGlobalAsString write SetGlobalAsString;
    property GlobalAsInt[const Path: WideString]: Integer read GetGlobalAsInt write SetGlobalAsInt;
    property GlobalAsReal[const Path: WideString]: Double read GetGlobalAsReal write SetGlobalAsReal;
    property GlobalRef[const Path: WideString]: Pointer read GetGlobalRef write SetGlobalRef;
    procedure GlobalDel(const Path: WideString; const Key: WideString);

    // Access lists
    property ListItem[const List: Pointer; const I: Integer]: Pointer read GetListItem write SetListItem;
    property ListString[const List: Pointer; const I: Integer]: WideString read GetListString write SetListString;
    property ListRefItem[const List: Pointer; const I: Integer]: Pointer read GetListRefItem write SetListRefItem;
    property ListItemByObjectName[const List: Pointer; const Name: WideString]: Pointer read GetListItemByObjectName;
    property ListRefValueByObjectName[const List: Pointer; const Name: WideString]: Pointer read GetListRefValueByObjectName;

    function ListCount(List: Pointer): Integer;
    procedure ListInsert(List: Pointer; Index: Integer; Value: Pointer; IncreaseRefCount: Boolean = True);
    procedure ListAdd(List: Pointer; Value: Pointer; IncreaseRefCount: Boolean = True);
    procedure ListAddString(List: Pointer; Name: WideString);
    function ListDel(List: Pointer; Index: Integer): Boolean; overload;
    function ListDel(List: Pointer; Value: Pointer): Boolean; overload;
    function ListDelString(List: Pointer; Name: WideString): Boolean;
    function ListDelObject(List: Pointer; Name: WideString): Boolean;
    function ListNew(const ContentType: GrtValueType; const ContentStructName: WideString = ''): Pointer;
    procedure ListClear(List: Pointer);

    // Access dicts
    property DictItem[const Dict: Pointer; const Key: WideString]: Pointer read GetDictItem write SetDictItem;
    property DictString[const Dict: Pointer; const Key: WideString]: WideString read GetDictString write SetDictString;
    property DictInt[const Dict: Pointer; const Key: WideString]: Integer read GetDictInt write SetDictInt;
    property DictReal[const Dict: Pointer; const Key: WideString]: Double read GetDictReal write SetDictReal;
    property DictRef[const Dict: Pointer; const Key: WideString]: Pointer read GetDictRef write SetDictRef;
    property DictStructName[const Dict: Pointer]: WideString read GetDictStructName write SetDictStructName;
    property DictKey[const Dict: Pointer; const I: Integer]: WideString read GetDictKey;
    function DictNew(const StructName: WideString): Pointer;
    function DictNewTyped(const ContentType: GrtValueType;
      const ContentStructName: WideString): Pointer;
    function DictItemCount(const Dict: Pointer): Integer;
    procedure DictDel(const Dict: Pointer; const Key: WideString);

    function ObjectNew(const StructName: WideString;
      Name: WideString; Id: WideString; OwnerId: WideString): Pointer;

    // Simple Values
    property ValueInt[const Value: Pointer]: Integer read GetValueInt;
    property ValueString[const Value: Pointer]: WideString read GetValueString;
    function ValueFromString(S: WideString): Pointer;
    function ValueFromInt(I: Integer): Pointer;

    procedure ValueRetain(Value: Pointer);
    procedure ValueRelease(Value: Pointer);
    function ValueType(Value: Pointer): GrtValueType;
    function ValueDuplicate(Value: Pointer): Pointer;
    function ValueLoadFromFile(FileName: WideString): Pointer;
    procedure ValueSaveToFile(FileName: WideString; Value: Pointer);
    procedure ValueListenerAdd(Value: Pointer; CallbackObject: IGrtValueCallbackListener);
    procedure ValueListenerRemove(Value: Pointer; CallbackObject: IGrtValueCallbackListener);
    function ValueAsLuaCode(Value: Pointer; Indention: Integer): WideString;
    function ValueDiffMake(Source: Pointer; Target: Pointer): Pointer;
    function ValueDiffApply(Value: Pointer; Diff: Pointer): Pointer;
    function ValueReference(Value: Pointer): Pointer; overload;
    function ValueReference(Id: WideString): Pointer; overload;

    function GlobalSetRoot(Value: Pointer): Integer;

    // Structs / Members

    function GetStructCaption(StructName: WideString): WideString;
    function GetStructMemberCount(StructName: WideString): Integer;
    function GetStructMemberName(StructName: WideString; Index: Integer): WideString;
    function GetStructMemberType(StructName: WideString; Index: Integer): GrtValueType; overload;
    function GetStructMemberType(Member: Pointer): GrtValueType; overload;
    function GetStructMemberStructName(StructName: WideString; MemberName: WideString): WideString;
    function GetStructMemberContentType(StructName: WideString; Index: Integer): GrtValueType;
    function GetStructMemberContentStructName(StructName: WideString; MemberName: WideString): WideString; overload;
    function GetStructMemberContentStructName(StructName: WideString; Index: Integer): WideString; overload;
    function StructExists(StructName: WideString): Boolean;
    function StructInheritsFrom(StructName: WideString; ParentStructName: WideString): Boolean;
    function StructIsOrInheritsFrom(StructName: WideString; ParentStructName: WideString): Boolean;

    procedure AddTask(Task: IGrtGenericTask);
    procedure AddTaskAndWait(Task: IGrtGenericTask; ShowError: Boolean);
    procedure AddTaskAndWaitWithMessages(Task: IGrtGenericTask; ShowError: Boolean);
    function CreateStandardTask(Description, ModulName, FunctionName: WideString; FunctionArguments: array of const;
      ProcessOutputFunction: TGrtProcessOutput = nil; ProcessMessagesFunction: TGrtProcessMessages = nil;
      AllowNullAsResult: Boolean = False; SearchParent: Boolean = False; Timeout: Integer = -1;
      PerformInputFunction: TGrtPerformInput = nil; PerformStatusQueryFunction: TGrtPerformStatusQuery = nil;
      TaskFinishedCallback: TGrtTaskFinished = nil; TaskTag: Integer = 0;
      DataObj: TObject = nil): IGrtTask;
    function CreateShellTask(Command: WideString; ProcessOutputFunction: TGrtProcessOutput = nil;
      Timeout: Integer = -1; TaskFinishedCallback: TGrtTaskFinished = nil): IGrtShellTask;
    procedure ExecuteScriptTask(FileName: WideString; ShowError: Boolean; ProcessOutputFunction: TGrtProcessOutput = nil;
      TimeOutMS: Integer = -1);
    function ExecuteStandardTask(Description, ModulName, FunctionName: WideString; FunctionArguments: array of const;
      ShowError: Boolean): Pointer;
    procedure RemoveTask(Task: IGrtGenericTask);

    function GetGlobalAsParam(Path: WideString): WideString;

    procedure AddDelphiModule(ModuleName: WideString; ModuleFunctions: WideString;
      Implementor: IGrtModuleImplementor);
    procedure RemoveDelphiModule(ModuleName: WideString;
      Implementor: IGrtModuleImplementor);

    function FunctionSuccess(Value: Pointer): Pointer;
    function FunctionError(ErrorString: WideString; ErrorDetails: WideString = ''): Pointer;

    property OutputBuffer: WideString read GetOutputBuffer write SetOutputBuffer;

    // Caches
    property StructIconsList: TTntStringList read FStructIconsList;

    // Helper functions
    function BuildGrtParamList(Params: array of const): Pointer;
    function FormatGrtMessagesAsString(Msgs: TMYX_GRT_MSGS): WideString;
    procedure Lock;
    procedure Unlock;
    procedure ScriptInputRead;

    property ShellVar[Name: WideString]: Pointer read GetShellVar write SetShellVar;

    procedure AddObjToReferenceCache(Dict: Pointer);
  end;

  TGrtClass = class of TGrt;

  PCallbackEntry = ^TCallbackEntry;
  TCallbackEntry = record
    Type_: Integer;
    TextForOutput: WideString;
    Messages: TMYX_GRT_MSGS;
  end;

  // The task list is basically a thread safe queue (FIFO) for interfaces.
  // Duplicates are allowed.
  TGrtTaskList = class(TObject)
  private
    FList: TInterfaceList;
    FLock: TRTLCriticalSection;
    function GetEmpty: Boolean;
    procedure SetEmpty(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function  LockList: TInterfaceList;
    function Pop: IGrtGenericTask;
    procedure Push(Task: IGrtGenericTask);
    procedure Remove(Task: IGrtGenericTask);
    procedure UnlockList;

    property Empty: Boolean read GetEmpty write SetEmpty;
  end;

  TGrtEngine = class(TThread)
  private
    FGrt: TGrt;
    FNativeGrt: Pointer;
    FWorkQueue: TGrtTaskList;
    FWorkEvent: TEvent;
    FCurrentTask: IGrtGenericTask;

    FDelphiModuleName,
    FDelphiModuleFunctionName: WideString;
    FDelphiModuleImplementor: IGrtModuleImplementor;
    FDelphiModuleFunctionArgument,
    FDelphiModuleFunctionResult: Pointer;

    FTextForOutput: WideString;
    FStatus: Integer;

    FTextInput: WideString;

    FJavaLoaderState,
    FLuaLoaderState,
    FBuiltinLoaderState,
    {$ifdef ENABLE_PHP_MODULES}
    FPhpLoaderState: GrtLoaderState;
    {$endif}
    FUiLoaderState: GrtLoaderState;
    FDelphiGrtMessages: TMYX_GRT_MSGS;
    FInputBuffer: Array [0 .. 160] of Char;
    FOutputBuffer: WideString;
    FNativeLoader: Pointer;
    FCallbackEntries: TThreadList;
  protected

    procedure ClearCallbackEntries;
    procedure AddCallbackEntry(AType: Integer; OutputText: WideString; Messages: PMYX_GRT_MSGS);
    procedure DoChangeCallback;
    function ApplicationHook(var Message: TMessage): Boolean; virtual;
    procedure Execute; override;

    procedure InitializeGrt;
    procedure InitializeLoadersAndModules; virtual;

    procedure FinalizeGrt;
    procedure FinalizeLoadersAndModules; virtual;

    function GetNativeGrt: Pointer;

    procedure DoOutputText(S: WideString);
    procedure DoProcessMessages(Messages: TMYX_GRT_MSGS);
    procedure DoGrtInputText;
    procedure DoStatusQuery;

    function InitDelphiLoader(Error: PMYX_GRT_ERROR): Pointer;

    procedure ExecuteDelphiFunction;

    procedure OutputModuleStatus(LoaderName: WideString; LoadedModuleCount: Integer; Error: MYX_GRT_ERROR);
    procedure TaskFinished;
  public
    constructor Create(CreateSuspended: Boolean; Grt: TGrt); virtual;
    destructor Destroy; override;

    property Grt: TGrt read FGrt;
    property NativeGrt: Pointer read GetNativeGrt;
    property OutputBuffer: WideString read FOutputBuffer write FOutputBuffer;

    property DelphiGrtMessages: TMYX_GRT_MSGS read FDelphiGrtMessages;

    procedure AddTask(Task: IGrtGenericTask);
    procedure AddTaskAndWait(Task: IGrtGenericTask);
    procedure AddTaskAndWaitWithMessages(Task: IGrtGenericTask);
    procedure RemoveTask(Task: IGrtGenericTask);

    procedure OutputText(S: WideString);
    function ProcessMessages(PMessages: PMYX_GRT_MSGS): Integer;
    function GrtInputText(Caption: WideString; options: MYX_GRT_INPUT_OPTIONS; Text: PPointer): integer;
    function GrtStatusQuery: Integer;

    procedure AddDelphiModule(ModuleName: WideString; ModuleFunctions: WideString;
      Implementor: IGrtModuleImplementor);
    procedure RemoveDelphiModule(ModuleName: WideString;
      Implementor: IGrtModuleImplementor);
  end;

  EGrtError = class(Exception)
  private
    FErrorNumber: Integer;
    FDescription: WideString;
  public
    constructor Create(ErrorNumber: Integer; Description: WideString);

    property ErrorNumber: Integer read FErrorNumber;
    property Description: WideString read FDescription;
  end;

function GetListMemberCount(Grt: Pointer; StructName: WideString; OnlyCheck: Boolean): Integer;
function GetListMember(Grt: Pointer; StructName: WideString; Index: Integer): Pointer;
function FormatGrtMessagesAsString(Msgs: TMYX_GRT_MSGS): WideString;
function GetGrtFunctionArgumentAsString(Argument: Pointer): WideString;

function RuntimeEnvironment: TGrt;
procedure SetGrtClass(AClass: TGrtClass);

procedure InitThreads;

//----------------------------------------------------------------------------------------------------------------------

implementation

var
  GrtClass: TGrtClass;        // Determines which actual class to use for the runtime environment.
  InternalGrtInstance: TGrt;

const
  WM_PROCESS_CALLBACK = WM_APP + 10001;

type
  TGrtGenericTask = class(TInterfacedObject, IGrtGenericTask)
  private
    FGrt: TGrt;
    FTaskID: Integer;
    FTimeOut: Integer;
    FExecutionTime: Cardinal; // Execution time in milliseconds.
    FWaitEvent: TEvent;
    FAllowNullResult: Boolean;
    FErrorString: WideString;
    FErrorCode: MYX_GRT_ERROR;
    FTag: Integer;
    FDataObj: TObject;

    FOnProcessOutput: TGrtProcessOutput;
    FOnTaskFinished: TGrtTaskFinished;

    function GetAllowNullResult: Boolean;
    function GetDataObj: TObject;
    function GetErrorCode: MYX_GRT_ERROR;
    function GetErrorString: WideString;
    function GetExecutionTime: Cardinal;
    function GetProcessOutputFunction: TGrtProcessOutput;
    function GetTag: Integer;
    function GetTaskFinishedFunction: TGrtTaskFinished;
    function GetTaskID: Integer;
    function GetTimeout: Integer;
    procedure SetErrorCode(const Value: MYX_GRT_ERROR);
    procedure SetErrorString(const Value: WideString);
    procedure SetEvent;
    procedure SetExecutionTime(const Value: Cardinal);
    procedure SetTag(Tag: Integer);
  public
    destructor Destroy; override;

    procedure WaitFor(WithMessages: Boolean);
    procedure PrepareSynchronization;
  end;

  TGrtTask = class(TGrtGenericTask, IGrtTask)
  private
    FDescription: WideString;
    FModulName: WideString;
    FFunctionName: WideString;
    FFunctionArguments: Pointer;
    FSearchParent: Boolean;
    FResult: Pointer;

    FONProcessMessages: TGrtProcessMessages;
    FOnPerformInput: TGrtPerformInput;
    FOnPerformStatusQuery: TGrtPerformStatusQuery;

    function GetDescription: WideString;
    function GetFunctionArgument: Pointer;
    function GetFunctionName: WideString;
    function GetModulName: WideString;
    function GetPerformInputFunction: TGrtPerformInput;
    function GetPerformStatusQueryFunction: TGrtPerformStatusQuery;
    function GetProcessMessagesFunction: TGrtProcessMessages;
    function GetResult: Pointer;
    function GetSearchParent: Boolean;
    procedure SetResult(const Value: Pointer);
  public
    destructor Destroy; override;
  end;

  TGrtShellTask = class(TGrtGenericTask, IGrtShellTask)
  private
    FCommand: WideString;
    FResult: MYX_GRT_SHELL_COMMAND;

    FOnProcessOutput: TGrtProcessOutput;

    function GetCommand: WideString; 
    function GetResult: MYX_GRT_SHELL_COMMAND;
    procedure SetResult(const Value: MYX_GRT_SHELL_COMMAND);
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure InitThreads;

begin
  myx_grt_init_threads();
end;

//----------------------------------------------------------------------------------------------------------------------

function RuntimeEnvironment: TGrt;

begin
  if (InternalGrtInstance = nil) then
    InternalGrtInstance := GrtClass.Create(nil);

  Result := InternalGrtInstance;
end;

//----------------------------------------------------------------------------------------------------------------------

function GrtValueCallback(grt: Pointer; value: Pointer; reason: MYX_GRT_VALUE_CALLBACK_REASON; user_data: Pointer): Integer; cdecl;

var
  Obj: IGrtValueCallbackListener;

begin
  Obj := IGrtValueCallbackListener(user_data);

  Result := Obj.ValueChange(value, GrtValueCallbackReason(reason));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ProcessGrtOutput(text: PChar; userdata: Pointer) cdecl;

var
  GrtEngine: TGrtEngine;
  S: WideString;

begin
  GrtEngine := userdata;
  S := UTF8Decode(text);

  GrtEngine.OutputText(S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ProcessGrtMessages(PMessages: PMYX_GRT_MSGS; userdata: Pointer) cdecl;

var
  GrtEngine: TGrtEngine;

begin
  GrtEngine := userdata;

  GrtEngine.ProcessMessages(PMessages);
end;

//----------------------------------------------------------------------------------------------------------------------

function ProcessInput(caption: PChar; options: MYX_GRT_INPUT_OPTIONS; text: PPointer; user_data: Pointer): Integer cdecl;

var
  GrtEngine: TGrtEngine;
  S: WideString;

begin
  GrtEngine := user_data;
  S := UTF8Decode(caption);

  Result := GrtEngine.GrtInputText(S, options, text);
end;

//----------------------------------------------------------------------------------------------------------------------

function ProcessStatusQuery(user_data: Pointer): Integer cdecl;

var
  GrtEngine: TGrtEngine;

begin
  GrtEngine := user_data;

  Result := GrtEngine.GrtStatusQuery();
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TGrt.Create(Console: TUniCodeConsole);

var
  TimeStart: TDateTime;
  TimeOut: Boolean;
  TimeOutInterval: TDateTime;
  WaitResult: Cardinal;
  WaitHandle: THandle;
  I: Integer;
  InitScriptFile: WideString;

begin
  inherited Create;

  FStructIconsList := TTntStringList.Create;
  FAccessLock := TCriticalSection.Create;
  FOnShutDown := nil;

  I := 1;

  // get startup arguments from command line
  Verbose := False;
  RemoteDebug := False;
  JvmLibrary := '';
  InitScriptFile := '';
  FShellInterface := 'lua';
  FJavaClasspath := 'java';
  JvmMaxHeap := '';

  while (I <= WideParamCount) do
  begin
    if (WideSameText(WideParamStr(I), '-verbose')) then
      FVerbose := True
    else
      if (WideSameText(WideParamStr(I), '-debug')) then
        FRemoteDebug := True
      else
        if (WideSameText(WideParamStr(I), '-py')) then
          FShellInterface := 'python'
        else
          if (WideSameText(WideParamStr(i), '-jvm')) and
            (i < WideParamCount) then
          begin
            FJvmLibrary := WideParamStr(i + 1);
            inc(i);
          end
          else
            if (WideSameText(WideParamStr(i), '-classpath')) and
              (i < WideParamCount) then
            begin
              FJavaClasspath := WideExcludeTrailingPathDelimiter(
                WideParamStr(i + 1));
              inc(i);
            end
            else
              if (WideSameText(WideParamStr(i), '-initscript')) and (i < ParamCount) then
              begin
                InitScriptFile := WideParamStr(i + 1);
                inc(i);
              end
              else
                if (Length(WideParamStr(i)) > 4) and
                  (WideSameText(Copy(WideParamStr(i), 1, 4), '-Xmx')) then
                begin
                  JvmMaxHeap := WideParamStr(i);
                end
                else
                  if (WideSameText(WideParamStr(i), '-Xmx')) and
                    (i < WideParamCount) then
                  begin
                    JvmMaxHeap := '-Xmx' + WideParamStr(i + 1);
                    inc(i);
                  end;

    inc(I);
  end;

  FConsole := Console;

  GrtSyncEvent := TEvent.Create(nil, False, False, '');

  FGrtEngine := CreateGrtEngine;
  FGrtEngine.FreeOnTerminate := False;

  // Wait for 20 sek
  TimeOut := False;
  TimeStart := Now;
  TimeOutInterval := (1 / 86400) * 20 * 10 * (1 * 10000);
  WaitHandle := GrtSyncEvent.Handle;

  while (not (TimeOut)) do
  begin
    // Wait for the initialisation to finish but every
    // 100 milliseonds check if the timeout value has been reached.
    // Process any incomming message while we wait.
    WaitResult := MsgWaitForMultipleObjects(
      1, WaitHandle, false, 100, QS_ALLEVENTS);
    if WaitResult = WAIT_OBJECT_0 then
      Break;

    Application.ProcessMessages;

    if (Now - TimeStart > TimeOutInterval) then
      TimeOut := True;
  end;

  if (TimeOut) then
    raise Exception.Create(_('Could not initialize the GRT Environment. ' +
      'A timeout occured during the initalization.'));

  GrtSyncEvent.Free;
  GrtSyncEvent := nil;

  if (InitScriptFile <> '') and (FGrtEngine.NativeGrt <> nil) then
    myx_grt_shell_run_file(FGrtEngine.NativeGrt, InitScriptFile, 1);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGrt.Destroy;

var
  I: Integer;

begin
  if (Assigned(FOnShutDown)) then
    FOnShutDown(self);

  //myx_grt_value_release(myx_grt_get_root(NativeGrt));

  // Free FGrtStructIconsList
  for I := 0 to FStructIconsList.Count - 1 do
    FStructIconsList.Objects[I].Free;
  FStructIconsList.Free;

  FGrtEngine.Terminate;
  FGrtEngine.WaitFor;
  FGrtEngine.Free;
  FAccessLock.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.CreateGrtEngine: TGrtEngine;

begin
  Result := TGrtEngine.Create(False, self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetJavaLoaderState: GrtLoaderState;

begin
  Result := FGrtEngine.FJavaLoaderState;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetLuaLoaderState: GrtLoaderState;

begin
  Result := FGrtEngine.FLuaLoaderState;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetBuiltinLoaderState: GrtLoaderState;

begin
  Result := FGrtEngine.FBuiltinLoaderState;
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifdef ENABLE_PHP_MODULES}
function TGrt.GetPhpLoaderState: GrtLoaderState;

begin
  Result := FGrtEngine.FUiLoaderState;
end;
{$endif}

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetUiLoaderState: GrtLoaderState;

begin
  Result := FGrtEngine.FUiLoaderState;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.OutputCommandLine(Text: WideString);

begin
  if Assigned(FConsole) then
    FConsole.AddOutput(Text + #13#10);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GrtNativeGrt: Pointer;

begin
  Result := FGrtEngine.NativeGrt;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.AddTask(Task: IGrtGenericTask);

// Adds the given task to the task list of the underlying Grt engine.
// It is allowed to add the same task more than once.

begin
  FGrtEngine.AddTask(Task);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.AddTaskAndWait(Task: IGrtGenericTask; ShowError: Boolean);

// Works like AddTask except that the caller is blocked until the task is finished.
// The value given as Timeout during task creation can be used to either block undefinedly (-1) or a certain amount.

var
  StandardTask: IGrtTask;

begin
  FGrtEngine.AddTaskAndWait(Task);

  if ShowError and ((Task.ErrorCode <> MYX_GRT_NO_ERROR) or (Task.ErrorString <> '')) then
    if Supports(Task, IGrtTask, StandardTask) then
      raise EGrtError.Create(Ord(Task.ErrorCode), StandardTask.ModuleName + '.' + StandardTask.FunctionName + ' :' +
        Task.ErrorString)
    else
      raise EGrtError.Create(Ord(Task.ErrorCode), Task.ErrorString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.AddTaskAndWaitWithMessages(Task: IGrtGenericTask; ShowError: Boolean);

// Works like AddTaskAndWait except that the applications can process its messages.
// This is a potential reentrancy problem and therefor this method should only be used by old code.
// Instead use the asynchronous way to wait for a task to finish.

var
  StandardTask: IGrtTask;

begin
  FGrtEngine.AddTaskAndWaitWithMessages(Task);

  if ShowError and ((Task.ErrorCode <> MYX_GRT_NO_ERROR) or (Task.ErrorString <> '')) then
    if Supports(Task, IGrtTask, StandardTask) then
      raise EGrtError.Create(Ord(Task.ErrorCode), StandardTask.ModuleName + '.' + StandardTask.FunctionName + ' :' +
        Task.ErrorString)
    else
      raise EGrtError.Create(Ord(Task.ErrorCode), Task.ErrorString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.CreateStandardTask(Description, ModulName, FunctionName: WideString; FunctionArguments: array of const;
      ProcessOutputFunction: TGrtProcessOutput; ProcessMessagesFunction: TGrtProcessMessages;
      AllowNullAsResult: Boolean; SearchParent: Boolean; Timeout: Integer; PerformInputFunction: TGrtPerformInput;
      PerformStatusQueryFunction: TGrtPerformStatusQuery; TaskFinishedCallback: TGrtTaskFinished;
      TaskTag: Integer; DataObj: TObject): IGrtTask;

// Creates a new task entry filled with the given values. The task is not yet added to the internal task list
// of the underlying GRT engine thread. Use AddTask for this.

var
  Task: TGrtTask;

begin
  Task := TGrtTask.Create;
  Task.FGrt := Self;

  Lock;
  Task.FTaskID := FNextTaskID;
  Inc(FNextTaskID);
  Unlock;

  Task.FDescription := Description;
  Task.FExecutionTime := 0;
  Task.FModulName := ModulName;
  Task.FFunctionName := FunctionName;
  Task.FFunctionArguments := BuildGrtParamList(FunctionArguments);
  Task.FSearchParent := SearchParent;
  Task.FTimeOut := TimeOut;
  Task.FAllowNullResult := AllowNullAsResult;

  Task.FOnProcessOutput := ProcessOutputFunction;
  Task.FOnProcessMessages := ProcessMessagesFunction;
  Task.FOnPerformInput := PerformInputFunction;
  Task.FOnPerformStatusQuery := PerformStatusQueryFunction;
  Task.FOnTaskFinished := TaskFinishedCallback;
  Task.FTag := TaskTag;
  Task.FDataObj := DataObj;

  Task.FResult := nil;
  Result := Task; // Returns the interface, sets the current ref count to 1 and starts so the life cycle of the task.
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.CreateShellTask(Command: WideString; ProcessOutputFunction: TGrtProcessOutput;
  Timeout: Integer; TaskFinishedCallback: TGrtTaskFinished): IGrtShellTask;

// Creates a new task entry filled with the given values. The task is not yet added to the internal task list
// of the underlying GRT engine thread. Use AddTask for this.
// This method is especially for shell tasks as they need fewer parameters.

var
  Task: TGrtShellTask;

begin
  Task := TGrtShellTask.Create;
  with Task do
  begin
    FGrt := Self;

    Lock;
    FTaskID := FNextTaskID;
    Inc(FNextTaskID);
    Unlock;

    FExecutionTime := 0;
    FTimeOut := TimeOut;
    FCommand := Command;

    FOnProcessOutput := ProcessOutputFunction;
    FOnTaskFinished := TaskFinishedCallback;

    FResult := MYX_GRT_SHELL_COMMAND_UNKNOWN;
  end;
  Result := Task; // Returns the interface, sets the current ref count to 1 and starts so the life cycle of the task.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ExecuteScriptTask(FileName: WideString; ShowError: Boolean; ProcessOutputFunction: TGrtProcessOutput;
  TimeOutMS: Integer);

var
  Script: TTntStringList;
  Task: IGrtShellTask;
  
begin
  if WideFileExists(FileName) then
  begin
    Script := TTntStringList.Create;
    try
      Script.LoadFromFile(FileName);
      if Script.Count > 0 then
      begin
        Task := CreateShellTask(Script.Text, ProcessOutputFunction, TimeOutMS);
        AddTaskAndWait(Task, ShowError);
      end;
    finally
      Script.Free;
    end;
  end
  else
    ShowModalDialog(_('Script file not found'), Format(_('The script file %s cannot be found.'), [FileName]),
      myx_mtError);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ExecuteStandardTask(Description, ModulName, FunctionName: WideString; FunctionArguments: array of const;
  ShowError: Boolean): Pointer;

// Simplified task creation and execution function to be used for tasks without callbacks/events.
// This method always waits until the task is finished.

var
  Task: IGrtTask;

begin
  Task := CreateStandardTask(Description, ModulName, FunctionName, FunctionArguments);
  AddTaskAndWait(Task, ShowError);
  Result := Task.Result;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.RemoveTask(Task: IGrtGenericTask);

// Removes the given task from the task list of the underlying GRT engine thread.
// It does not harm if the task is not in that list.

begin
  FGrtEngine.RemoveTask(Task);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.BuildGrtParamList(Params: array of const): Pointer;

var
  ParamList: Pointer;
  Param: Pointer;
  I: Integer;

begin
  ParamList := myx_grt_list_new(MYX_ANY_VALUE, '');

  for I := 0 to High(Params) do
  begin
    with Params[I] do
      case VType of
        vtInteger:
          Param := myx_grt_value_from_int(VInteger);
        vtExtended:
          Param := myx_grt_value_from_real(VExtended^);
        vtWideString:
          Param := myx_grt_value_from_string(WideString(VWideString));
        vtString:
          Param := myx_grt_value_from_string(VString^);
        vtAnsiString:
          Param := myx_grt_value_from_string(string(VAnsiString));
        vtChar:
          Param := myx_grt_value_from_string(VChar);
        vtPChar:
          Param := myx_grt_value_from_string(VPChar);
        vtPointer:
          Param := VPointer;
        vtObject:
          Param := VObject;
      else
        raise EInOutError.Create(_('BuildGrtParamList called with unsupported parameter type.'));
      end;

    myx_grt_list_item_add(ParamList, Param);

    if not (Params[I].VType in [vtPointer, vtObject]) then
      ValueRelease(Param);
  end;

  Result := ParamList;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.AddDelphiModule(ModuleName: WideString; ModuleFunctions: WideString;
  Implementor: IGrtModuleImplementor);

begin
  FGrtEngine.AddDelphiModule(ModuleName, ModuleFunctions, Implementor);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.RemoveDelphiModule(ModuleName: WideString;
  Implementor: IGrtModuleImplementor);

begin
  FGrtEngine.RemoveDelphiModule(ModuleName, Implementor);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.FunctionSuccess(Value: Pointer): Pointer;

begin
  Result := DictNew('');

  DictItem[Result, 'value'] := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.FunctionError(ErrorString: WideString; ErrorDetails: WideString): Pointer;

begin
  Result := DictNew('');

  DictItem[Result, 'error'] := ValueFromString(ErrorString);
  if (ErrorDetails <> '') then
    DictItem[Result, 'detail'] := ValueFromString(ErrorDetails);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetGlobal(const Path: WideString): Pointer;

begin
  Result := myx_grt_dict_item_get_by_path(NativeGrt, myx_grt_get_root(NativeGrt), Path);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetGlobal(const Path: WideString; NewValue: Pointer);

begin
  if (myx_grt_dict_item_set_by_path(myx_grt_get_root(NativeGrt), Path, NewValue) <> 0) then
    raise Exception.Create(Format(_('The value %s cannot be set.'), [Path]));
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetGlobalAsString(const Path: WideString): WideString;

var
  PValue: Pointer;

begin
  PValue := Global[Path];

  if (PValue <> nil) then
  begin
    if (myx_grt_value_get_type(PValue) <> MYX_STRING_VALUE) then
      raise Exception.Create(Format(
        _('The value %s is not a string value.'),
        [Path]));

    Result := myx_grt_value_as_string(PValue);
  end
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetGlobalAsString(const Path: WideString; NewValue: WideString);

begin
  Global[Path] := myx_grt_value_from_string(NewValue);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetGlobalAsInt(const Path: WideString): Integer;

var
  PValue: Pointer;

begin
  PValue := Global[Path];

  if (PValue <> nil) then
  begin
    if (myx_grt_value_get_type(PValue) <> MYX_INT_VALUE) then
      raise Exception.Create(Format(
        _('The value %s is not a int value.'),
        [Path]));

    Result := myx_grt_value_as_int(PValue);
  end
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetGlobalAsInt(const Path: WideString; NewValue: Integer);

begin
  Global[Path] := myx_grt_value_from_int(NewValue);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetGlobalAsReal(const Path: WideString): Double;

var
  PValue: Pointer;

begin
  PValue := Global[Path];

  if (PValue <> nil) then
  begin
    if (myx_grt_value_get_type(PValue) <> MYX_INT_VALUE) then
      raise Exception.Create(Format(
        _('The value %s is not a real value.'),
        [Path]));

    Result := myx_grt_value_as_real(PValue);
  end
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetGlobalAsReal(const Path: WideString; NewValue: Double);

begin
  Global[Path] := myx_grt_value_from_real(NewValue);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetGlobalRef(const Path: WideString): Pointer;

var
  Id: WideString;

begin
  Result := nil;

  Id := GetGlobalAsString(Path);
  if (Id <> '') then
    Result := myx_grt_reference_cache_lookup(NativeGrt, Id);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetGlobalRef(const Path: WideString; NewValue: Pointer);

begin
  Global[Path] := DictItem[NewValue, '_id'];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.GlobalDel(const Path: WideString; const Key: WideString);

var
  Value: Pointer;

begin
  Value := myx_grt_dict_item_get_by_path(NativeGrt, myx_grt_get_root(NativeGrt), Path);

  if (ValueType(Value) = GrtDictValue) then
    myx_grt_dict_item_del(Value, Key)
  else
    if (ValueType(Value) = GrtListValue) then
      myx_grt_list_item_del(Value, StrToInt(Key));
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueType(Value: Pointer): GrtValueType;

begin
  Result := GrtValueType(myx_grt_value_get_type(Value));
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueDuplicate(Value: Pointer): Pointer;

begin
  Result := myx_grt_value_dup(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueLoadFromFile(FileName: WideString): Pointer;

begin
  Result := myx_grt_retrieve_from_file(NativeGrt, FileName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ValueSaveToFile(FileName: WideString; Value: Pointer);

var
  Error: MYX_GRT_ERROR;

begin
  Error := myx_grt_store_to_file(NativeGrt, Value, FileName);

  if (Error <> MYX_GRT_NO_ERROR) then
    raise Exception.CreateFmt(
      _('The following error occured. Error Nr. %d'), [Ord(Error)]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ValueListenerAdd(Value: Pointer; CallbackObject: IGrtValueCallbackListener);
begin
  myx_grt_value_listener_add(NativeGrt, Value, Pointer(CallbackObject), @GrtValueCallback);

  // Add a reference because a pointer to the interfaces is store in the GRT
  CallbackObject._AddRef;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ValueListenerRemove(Value: Pointer; CallbackObject: IGrtValueCallbackListener);

begin
  myx_grt_value_listener_remove(Value, Pointer(CallbackObject), @GrtValueCallback);

  // Release refcount since the interface ref is now no longer stored in the GRT
  CallbackObject._Release;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GlobalSetRoot(Value: Pointer): Integer;

begin
  Result := Ord(myx_grt_set_root(NativeGrt, Value));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ValueRetain(Value: Pointer);

begin
  myx_grt_value_retain(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ValueRelease(Value: Pointer);

begin
  myx_grt_value_release(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructMemberCount(StructName: WideString): Integer;

var
  Struct: Pointer;

begin
  Struct := myx_grt_struct_get(NativeGrt, StructName);

  if (Struct <> nil) then
    Result := myx_grt_struct_get_member_count_total(NativeGrt, Struct)
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructMemberName(StructName: WideString; Index: Integer): WideString;

var
  Struct,
  Member: Pointer;

begin
  Result := '';

  Struct := myx_grt_struct_get(NativeGrt, StructName);

  if (Struct <> nil) then
  begin
    Member := myx_grt_struct_get_member_by_index_total(NativeGrt, Struct, Index);

    if (Member <> nil) then
      Result := myx_grt_struct_get_member_name(Member);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructMemberType(StructName: WideString; Index: Integer): GrtValueType;

var
  Struct,
  Member: Pointer;

begin
  Result := GrtAnyValue;

  Struct := myx_grt_struct_get(NativeGrt, StructName);

  if (Struct <> nil) then
  begin
    Member := myx_grt_struct_get_member_by_index_total(NativeGrt, Struct, Index);

    if (Member <> nil) then
      Result := GrtValueType(myx_grt_struct_member_get_type(Member));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructMemberType(Member: Pointer): GrtValueType;

begin
  Result := GrtAnyValue;

  if (Member <> nil) then
    Result := GrtValueType(myx_grt_struct_member_get_type(Member));
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructMemberContentType(StructName: WideString; Index: Integer): GrtValueType;

var
  Struct,
  Member: Pointer;

begin
  Result := GrtAnyValue;

  Struct := myx_grt_struct_get(NativeGrt, StructName);

  if (Struct <> nil) then
  begin
    Member := myx_grt_struct_get_member_by_index_total(NativeGrt, Struct, Index);

    if (Member <> nil) then
      Result := GrtValueType(myx_grt_struct_member_get_content_type(Member));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructMemberStructName(StructName: WideString; MemberName: WideString): WideString;

var
  Struct,
  Member: Pointer;

begin
  Result := '';

  Struct := myx_grt_struct_get(NativeGrt, StructName);

  if (Struct <> nil) then
  begin
    Member := myx_grt_struct_get_member_by_name(NativeGrt, Struct, MemberName, 1);

    if (Member <> nil) then
      Result := myx_grt_struct_member_get_struct_name(Member);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructMemberContentStructName(StructName: WideString; MemberName: WideString): WideString;

var
  Struct,
  Member: Pointer;

begin
  Result := '';

  Struct := myx_grt_struct_get(NativeGrt, StructName);

  if (Struct <> nil) then
  begin
    Member := myx_grt_struct_get_member_by_name(NativeGrt, Struct, MemberName, 1);

    if (Member <> nil) then
      Result := myx_grt_struct_member_get_content_struct_name(Member);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructMemberContentStructName(StructName: WideString; Index: Integer): WideString;

var
  Struct,
  Member: Pointer;

begin
  Result := '';

  Struct := myx_grt_struct_get(NativeGrt, StructName);

  if (Struct <> nil) then
  begin
    Member := myx_grt_struct_get_member_by_index_total(NativeGrt, Struct, Index);

    if (Member <> nil) then
      Result := myx_grt_struct_member_get_content_struct_name(Member);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetStructCaption(StructName: WideString): WideString;

var
  Struct: PMYX_GRT_STRUCT;
  InheritedCaption: Integer;

begin
  Struct := myx_grt_struct_get(NativeGrt, StructName);

  Result := myx_grt_struct_get_caption(NativeGrt, Struct, @InheritedCaption);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.StructExists(StructName: WideString): Boolean;

begin
  Result := (myx_grt_struct_get(NativeGrt, StructName) <> nil);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.StructInheritsFrom(StructName: WideString; ParentStructName: WideString): Boolean;

begin
  Result := (myx_grt_struct_inherits_from(NativeGrt, StructName, ParentStructName) = 1);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.StructIsOrInheritsFrom(StructName: WideString; ParentStructName: WideString): Boolean;

begin
  Result := (myx_grt_struct_is_or_inherits_from(NativeGrt, StructName, ParentStructName) = 1);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.FormatGrtMessagesAsString(Msgs: TMYX_GRT_MSGS): WideString;

begin
  Result := FormatGrtMessagesAsString(Msgs);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ListCount(List: Pointer): Integer;

begin
  Result := myx_grt_list_item_count(List);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetListItem(const List: Pointer; const I: Integer): Pointer;

begin
  Result := myx_grt_list_item_get(List, I);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetListItem(const List: Pointer; const I: Integer; NewValue: Pointer);

begin
  myx_grt_list_item_set(List, I, NewValue);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetListString(const List: Pointer; const I: Integer): WideString;

begin
  Result := myx_grt_list_item_get_as_string(List, I);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetListString(const List: Pointer; const I: Integer; NewValue: WideString);

var
  Value: Pointer;
  OrgValue: Pointer;
  ValueType: MYX_GRT_VALUE_TYPE;
  
begin
  // Check what the original value's type is and auto-convert the given string to a native type if possible.
  OrgValue := myx_grt_list_item_get(List, I);
  ValueType := myx_grt_value_get_type(OrgValue);
  case ValueType of
    MYX_INT_VALUE:
      try
        Value := myx_grt_value_from_int(StrToInt(NewValue));
      except
        raise Exception.Create(Format(_('The string %s cannot be converted to an integer value.'), [NewValue]));
      end;
    MYX_REAL_VALUE:
      try
        Value := myx_grt_value_from_real(StrToFloat(NewValue));
      except
        raise Exception.Create(Format(_('The string %s cannot be converted to a float value.'), [NewValue]));
      end;
  else
    // Everything else.
    Value := myx_grt_value_from_string(NewValue);
  end;
  myx_grt_list_item_set(List, I, Value);
  myx_grt_value_release(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetListRefItem(const List: Pointer; const I: Integer): Pointer;

begin
  Result := myx_grt_list_item_get_reference_value(NativeGrt, List, I);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetListRefItem(const List: Pointer; const I: Integer; NewValue: Pointer);

var
  Value: Pointer;

begin
  Value := myx_grt_value_from_string(DictString[NewValue, '_id']);

  myx_grt_list_item_set(List, I, Value);

  myx_grt_value_release(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetListItemByObjectName(const List: Pointer; const Name: WideString): Pointer;

begin
  Result := myx_grt_list_item_get_by_object_name(List, Name);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetListRefValueByObjectName(const List: Pointer; const Name: WideString): Pointer;

begin
  Result := myx_grt_list_item_get_reference_value_by_object_name(NativeGrt, List, Name);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ListAdd(List: Pointer; Value: Pointer; IncreaseRefCount: Boolean);

begin
  myx_grt_list_item_add(List, Value);

  // as myx_grt_list_item_add increases refcount,
  // release it one time if it should not be increased
  if (not (IncreaseRefCount)) then
    myx_grt_value_release(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ListAddString(List: Pointer; Name: WideString);

var
  Value: Pointer;

begin
  Value := ValueFromString(Name);

  ListAdd(List, Value, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ListInsert(List: Pointer; Index: Integer; Value: Pointer; IncreaseRefCount: Boolean);

begin
  myx_grt_list_item_insert(List, Index, Value);

  // as myx_grt_list_item_insert increases refcount,
  // release it one time if it should not be increased
  if (not (IncreaseRefCount)) then
    myx_grt_value_release(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ListDel(List: Pointer; Index: Integer): Boolean;

begin
  Result := (myx_grt_list_item_del(List, Index) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ListDel(List: Pointer; Value: Pointer): Boolean;

begin
  Result := (myx_grt_list_item_del_value(List, Value) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ListDelString(List: Pointer; Name: WideString): Boolean;

begin
  Result := (myx_grt_list_item_del_as_string(List, Name) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ListDelObject(List: Pointer; Name: WideString): Boolean;

begin
  Result := (myx_grt_list_del_by_object_name(List, Name) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ListNew(const ContentType: GrtValueType;
  const ContentStructName: WideString = ''): Pointer;

begin
  Result := myx_grt_list_new(MYX_GRT_VALUE_TYPE(ContentType),
    ContentStructName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ListClear(List: Pointer);

begin
  myx_grt_list_clear(List);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetDictItem(const Dict: Pointer; const Key: WideString): Pointer;

begin
  Result := myx_grt_dict_item_get_value(Dict, Key);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetDictItem(const Dict: Pointer; const Key: WideString; NewValue: Pointer);

begin
  myx_grt_dict_item_set_value(Dict, Key, NewValue);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetDictString(const Dict: Pointer; const Key: WideString): WideString;

begin
  if (Dict <> nil) then
    Result := myx_grt_dict_item_get_as_string(Dict, Key)
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetDictString(const Dict: Pointer; const Key: WideString; NewValue: WideString);

var
  Value: Pointer;
  OrgValue: Pointer;
  ValueType: MYX_GRT_VALUE_TYPE;

begin
  // Check what the original value's type is and auto-convert the given string to a native type if possible.
  OrgValue := myx_grt_dict_item_get_value(Dict, Key);
  ValueType := myx_grt_value_get_type(OrgValue);
  case ValueType of
    MYX_INT_VALUE:
      try
        Value := myx_grt_value_from_int(StrToInt(NewValue));
      except
        raise Exception.Create(Format(_('The string %s cannot be converted to an integer value.'), [NewValue]));
      end;
    MYX_REAL_VALUE:
      try
        Value := myx_grt_value_from_real(StrToFloat(NewValue));
      except
        raise Exception.Create(Format(_('The string %s cannot be converted to a float value.'), [NewValue]));
      end;
  else
    // Everything else.
    Value := myx_grt_value_from_string(NewValue);
  end;

  myx_grt_dict_item_set_value(Dict, Key, Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetDictInt(const Dict: Pointer; const Key: WideString): Integer;

begin
  Result := myx_grt_dict_item_get_as_int(Dict, Key);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetDictInt(const Dict: Pointer; const Key: WideString; NewValue: Integer);

begin
  myx_grt_dict_item_set_value_from_int(Dict, Key, NewValue);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetDictReal(const Dict: Pointer; const Key: WideString): Double;

begin
  Result := myx_grt_dict_item_get_as_real(Dict, Key);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetDictReal(const Dict: Pointer; const Key: WideString; NewValue: Double);

begin
  myx_grt_dict_item_set_value_from_real(Dict, Key, NewValue);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetDictRef(const Dict: Pointer; const Key: WideString): Pointer;

begin
  Result := myx_grt_dict_item_get_reference_value(FGrtEngine.NativeGrt, Dict, Key);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetDictRef(const Dict: Pointer; const Key: WideString; NewValue: Pointer);

begin
  myx_grt_dict_item_set_value_from_string(Dict, Key,
    DictString[NewValue, '_id']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetDictStructName(const Dict: Pointer): WideString;

begin
  Result := myx_grt_dict_struct_get_name(Dict);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetDictStructName(const Dict: Pointer; StructName: WideString);

begin
  myx_grt_dict_struct_set_name(NativeGrt, Dict, StructName);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.DictNew(const StructName: WideString): Pointer;

begin
  Result := myx_grt_dict_new(NativeGrt, StructName);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.DictNewTyped(const ContentType: GrtValueType;
  const ContentStructName: WideString): Pointer;

begin
  Result := myx_grt_dict_new_typed(MYX_GRT_VALUE_TYPE(ContentType),
    ContentStructName);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ObjectNew(const StructName: WideString;
  Name: WideString; Id: WideString; OwnerId: WideString): Pointer;

begin
  Result := myx_grt_dict_new_obj(NativeGrt, StructName, Name, Id, OwnerId);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetValueInt(const Value: Pointer): Integer;

begin
  Result := myx_grt_value_as_int(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetValueString(const Value: Pointer): WideString;

begin
  Result := myx_grt_value_as_string(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueFromString(S: WideString): Pointer;

begin
  Result := myx_grt_value_from_string(S);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueFromInt(I: Integer): Pointer;

begin
  Result := myx_grt_value_from_int(I);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueAsLuaCode(Value: Pointer; Indention: Integer): WideString;

begin
  Result := myx_grt_value_as_lua_code(Value, Indention);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueDiffMake(Source: Pointer; Target: Pointer): Pointer;

begin
  Result := myx_grt_value_diff_make(NativeGrt, Source, Target);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueDiffApply(Value: Pointer; Diff: Pointer): Pointer;

begin
  Result := myx_grt_value_diff_apply(NativeGrt, Value, Diff);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueReference(Value: Pointer): Pointer;

begin
  Result := myx_grt_reference_cache_lookup(NativeGrt,
    ValueString[Value]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.ValueReference(Id: WideString): Pointer;

begin
  Result := myx_grt_reference_cache_lookup(NativeGrt,
    PWideChar(Id));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.Lock;

// Sets a lock on the GRT class for the calling thread or holds it if another thread already has a lock.

begin
  FAccessLock.Acquire;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.Unlock;

// Releases the lock for the calling thread, set previously with Lock.

begin
  FAccessLock.Release;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.ScriptInputRead;

begin
  if (GrtSyncEvent <> nil) then
    GrtSyncEvent.SetEvent;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetOutputBuffer: WideString;

begin
  Result := FGrtEngine.OutputBuffer;

  FGrtEngine.OutputBuffer := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetOutputBuffer(OutputBuffer: WideString);

begin
  FGrtEngine.OutputBuffer := OutputBuffer;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetShellVar(Name: WideString): Pointer;

begin
  Result := myx_grt_shell_get_global_var(NativeGrt, Name);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.SetShellVar(Name: WideString; Value: Pointer);

begin
  myx_grt_shell_set_global_var(NativeGrt, Name, Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetGlobalAsParam(Path: WideString): WideString;

begin
  Result := 'global::' + Path;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.DictItemCount(const Dict: Pointer): Integer;

begin
  Result := myx_grt_dict_item_count(Dict);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.DictDel(const Dict: Pointer; const Key: WideString);

begin
  myx_grt_dict_item_del(Dict, Key);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrt.GetDictKey(const Dict: Pointer; const I: Integer): WideString;

begin
  Result := myx_grt_dict_item_key_by_index(Dict, I);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrt.AddObjToReferenceCache(Dict: Pointer);

begin
  myx_grt_reference_cache_add(NativeGrt, Dict);
end;


//----------------------------------------------------------------------------------------------------------------------
// Grt Engine
//----------------------------------------------------------------------------------------------------------------------

constructor TGrtEngine.Create(CreateSuspended: Boolean; Grt: TGrt);

begin
  inherited Create(CreateSuspended);

  FGrt := Grt;
  FNativeGrt := nil;

  FWorkQueue := TGrtTaskList.Create;

  FCallbackEntries := TThreadList.Create;

  FJavaLoaderState := GrtLsNotInitialized;
  FLuaLoaderState := GrtLsNotInitialized;
  FBuiltinLoaderState := GrtLsNotInitialized;
  {$ifdef ENABLE_PHP_MODULES}
  FPhpLoaderState := GrtLsNotInitialized;
  {$endif}
  FUiLoaderState := GrtLsNotInitialized;

  FWorkEvent := TEvent.Create(nil, False, False, '');

  FDelphiGrtMessages := TMYX_GRT_MSGS.Create;
  Application.HookMainWindow(ApplicationHook);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGrtEngine.Destroy;

begin
  Application.UnhookMainWindow(ApplicationHook);

  FWorkQueue.Free;

  ClearCallbackEntries;
  FCallbackEntries.Free;

  if FNativeGrt <> nil then
    myx_grt_finalize(FNativeGrt);

  FWorkEvent.Free;

  FDelphiGrtMessages.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.Execute;

var
  ErrorDetails: TMYX_STRINGLIST;
  GrtOptions: Integer;
  StandardTask: IGrtTask;
  ShellTask: IGrtShellTask;
  StartTicks: Cardinal;
  Error: MYX_GRT_ERROR;
  Result: Pointer;

begin
  GrtOptions := 0;
  if (FGrt.Verbose) then
    GrtOptions := GrtOptions + Ord(MYX_GRT_VERBOSE);
  if (FGrt.RemoteDebug) then
    GrtOptions := GrtOptions + Ord(MYX_GRT_REMOTE_DEBUG);

  FNativeGrt := myx_grt_initialize(GrtOptions);

  InitializeGrt;

  while not Terminated do
  begin
    // Wait at most one second it there is no work to do.
    // Check if the thread is being shut down and continue loop if not.
    if FWorkEvent.WaitFor(1000) = wrSignaled then
    begin
      // New work has arrived. Get it from the work queue. Check once more we really have work.
      if FWorkQueue.Empty then
        Continue;

      FCurrentTask := FWorkQueue.Pop;

      // Immediately set the work event if the work queue is not empty so we continously
      // execute all tasks until our work queue is empty.
      if not FWorkQueue.Empty then
        FWorkEvent.SetEvent;

      StartTicks := GetTickCount;
      try
        try
          if Supports(FCurrentTask, IGrtTask, StandardTask) then
          begin
            StandardTask.Result := nil;

            // Execute a standard task and check its result.
            Result := myx_grt_function_get_and_call(FNativeGrt, StandardTask.ModuleName, StandardTask.FunctionName,
              Ord(StandardTask.SearchParent), StandardTask.FunctionArgument, @Error);

            // Check the result for error keys.
            FCurrentTask.ErrorCode := Error;
            FCurrentTask.ErrorString := myx_grt_function_check_error(Result, Ord(FCurrentTask.AllowNullResult));

            // Release result if there was an error.
            if (Error <> MYX_GRT_NO_ERROR) or (FCurrentTask.ErrorString <> '') and Assigned(Result) then
            begin
              myx_grt_value_release(Result);
              Result := nil;
            end;

            // If there was no error we have a value key. Return the contents of that key as result.
            if Assigned(Result) then
            begin
              StandardTask.Result := myx_grt_dict_item_get_value(Result, 'value');
              myx_grt_value_retain(StandardTask.Result);

              // Release result wrapper dict.
              myx_grt_value_release(Result);
            end;
          end
          else
            if Supports(FCurrentTask, IGrtShellTask, ShellTask) then
            begin
              // A shell task is to be executed. Extract the shell result afterwards.
              ShellTask.Result := myx_grt_shell_execute(FNativeGrt,
                Tnt_WideStringReplace(ShellTask.Command, #13#10, #10,[rfReplaceAll]));
            end;

        finally
          FCurrentTask.ExecutionTime := GetTickCount - StartTicks;
          FCurrentTask.SetEvent;
          if Assigned(FCurrentTask.OnTaskFinished) then
            Synchronize(TaskFinished);
        end;
      except
        on X: Exception do
        begin
          ErrorDetails := TMYX_STRINGLIST.Create;
          try
            if Assigned(StandardTask) then
              ErrorDetails.strings.Text := Format(_('The call to the function %s:%s returned with an exception.'),
                [StandardTask.ModuleName, StandardTask.FunctionName])
            else
              if Assigned(ShellTask) then
                ErrorDetails.strings.Text := Format(_('Exception encountered while executing the shell command: "%s"'),
                  [ShellTask.Command])
              else
                ErrorDetails.strings.Text := _('An unknown error occured during execution of a GRT function.');
            myx_grt_messages_stack_add(FNativeGrt, 1, X.Message, ErrorDetails.get_record_pointer, 1, -1);
            myx_grt_messages_stack_flush(FNativeGrt, 0);
          finally
            ErrorDetails.Free;
          end;
        end;
      end;

      // Process pending text/msg output
      Application.ProcessMessages;
    end;
  end;
  FinalizeGrt;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.FinalizeGrt;

begin
  FGrt.ScriptInputRead;

  FinalizeLoadersAndModules;
end;

procedure TGrtEngine.FinalizeLoadersAndModules;

begin
  FGrt.ScriptInputRead;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.InitializeGrt;

var
  Error: MYX_GRT_ERROR;
  I: integer;

begin
  myx_grt_catch_glib_messages(FNativeGrt, 1);

  // setup shell
  if (FGrt.ShellInterface = 'python') then
    myx_grt_setup_shell(FNativeGrt, MYX_GRT_SHELL_PYTHON)
  else
    myx_grt_setup_shell(FNativeGrt, MYX_GRT_SHELL_LUA);

  // Register print output function
  myx_grt_set_output_callback(FNativeGrt, self, @ProcessGrtOutput);

  // Register message processing function
  myx_grt_set_message_callback(FNativeGrt, self, @ProcessGrtMessages);

  // Register input function
  myx_grt_set_input_callback(FNativeGrt, self, @ProcessInput);

  // Register status query function
  myx_grt_set_status_query_callback(FNativeGrt, self, @ProcessStatusQuery);

  myx_grt_shell_print_welcome(FNativeGrt);

  OutputText('');

  // -------------------------------------------------
  // Load Structs

  I := myx_grt_scan_for_structs(FNativeGrt, './xml', @Error);
  if (Error <> MYX_GRT_NO_ERROR) then
    OutputText('  ' + Format(
      _('Error while loading struct definitions (%d).') + #13#10,
      [Ord(Error)]))
  else
    if (I = 1) then
      OutputText(
        _('Registered 1 struct definition file.') + #13#10)
    else
      OutputText(Format(
        _('Registered %d struct definition files.') + #13#10,
        [I]));


  InitializeLoadersAndModules;


  OutputText(#13#10);

  //Init shell
  myx_grt_shell_init(FNativeGrt);

  FGrt.GrtSyncEvent.SetEvent;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.InitializeLoadersAndModules;

var
  Loader: Pointer;
  Error: MYX_GRT_ERROR;
  I: integer;

begin
  // -------------------------------------------------
  // Initialized Loaders

  // Init Delphi loader
  OutputText(_('Initializing native loader...') + #13#10);
  FUiLoaderState := GrtLsInitializeFailed;
  FNativeLoader := InitDelphiLoader(@Error);
  if (FNativeLoader <> nil) then
  begin
    if (myx_grt_register_module_loader(FNativeGrt, FNativeLoader) <> MYX_GRT_NO_ERROR) then
      OutputText('  ' + _('Could not register Delphi modules.') + #13#10)
    else
      FUiLoaderState := GrtLsInitialized;
  end
  else
    OutputText('  ' + Format(
      _('Error initializing Delphi modules (%d)') + #13#10,
      [Ord(error)]));

  // Init Java loader
  if (WideDirectoryExists(FGrt.FJavaClasspath)) then
  begin
    OutputText(_('Initializing Java loader...') + #13#10);
    if (FGrt.Verbose) then
      OutputText(#13#10);

    FJavaLoaderState := GrtLsInitializeFailed;

    Loader := myx_java_init_loader_advanced(FNativeGrt, FGrt.FJavaClasspath, @Error,
      FGrt.JvmLibrary, FGrt.FJavaClasspath, FGrt.JvmMaxHeap);

    if (Loader <> nil) then
    begin
      FJavaLoaderState := GrtLsInitialized;

      if (myx_grt_register_module_loader(FNativeGrt, Loader) <> MYX_GRT_NO_ERROR) then
        OutputText('  ' + _('Could not register Java module loader.') + #13#10)
      else
        FJavaLoaderState := GrtLsInitialized;
    end
    else
      OutputText('  ' + Format(
        _('Error initializing Java module loader (%d). ' +
          'Please check the .\java and .\java\lib directories.') + #13#10,
        [Ord(error)]));
  end;

  {$ifdef ENABLE_PHP_MODULES}
  // Init PHP loader
  if (WideDirectoryExists(GetApplDir + 'php')) then
  begin
    OutputText(_('Initializing PHP loader...') + #13#10);
    FPhpLoaderState := GrtLsInitializeFailed;

    Loader := myx_php_init_loader(FNativeGrt, @Error);
    if (Loader <> nil) then
    begin
      if (myx_grt_register_module_loader(FNativeGrt, Loader) <> MYX_GRT_NO_ERROR) then
        OutputText('  ' + _('Could not register PHP module loader.') + #13#10)
      else
        FPhpLoaderState := GrtLsInitialized;
    end
    else
      OutputText('  ' + Format(
        _('Error initializing PHP module loader (%d)') + #13#10,
        [Ord(error)]));
  end;
  {$endif}
  
  // Init lua loader
  OutputText(_('Initializing Lua loader...') + #13#10);
  FLuaLoaderState := GrtLsInitializeFailed;
  Loader := myx_lua_init_loader(FNativeGrt, @Error, './lua');
  if (Loader <> nil) then
  begin
    if (myx_grt_register_module_loader(FNativeGrt, Loader) <> MYX_GRT_NO_ERROR) then
      OutputText('  ' + _('Could not register Lua module loader.') + #13#10)
    else
      FLuaLoaderState := GrtLsInitialized;
  end
  else
    OutputText('  ' + Format(
      _('Error initializing Lua module loader (%d)') + #13#10,
      [Ord(error)]));

  // Init python loader
  OutputText(_('Initializing Python loader...') + #13#10);
  FLuaLoaderState := GrtLsInitializeFailed;
  Loader := myx_python_init_loader(FNativeGrt, @Error, './python');
  if (Loader <> nil) then
  begin
    if (myx_grt_register_module_loader(FNativeGrt, Loader) <> MYX_GRT_NO_ERROR) then
      OutputText('  ' + _('Could not register Python module loader.') + #13#10)
    else
      FLuaLoaderState := GrtLsInitialized;
  end
  else
    OutputText('  ' + Format(
      _('Error initializing Python module loader (%d)') + #13#10,
      [Ord(error)]));

  // -------------------------------------------------
  // Load modules

  //Load builtin modules
  FBuiltinLoaderState := GrtLsInitializeFailed;
  if (myx_register_builtin_grt_module_base(FNativeGrt) <> nil) and
    (myx_register_builtin_grt_module_reverse_engineer_mysql(FNativeGrt) <> nil) and
    (myx_register_builtin_grt_module_transformation_mysql(FNativeGrt) <> nil) {and
    (myx_register_builtin_grt_module_query_mysql(FNativeGrt) <> nil) and
    (myx_register_builtin_grt_module_result_set(FNativeGrt) <> nil)} then
  begin
    FBuiltinLoaderState := GrtLsModulesLoaded;

    OutputModuleStatus('builtin', 5, MYX_GRT_NO_ERROR);
  end;

  // Java modules
  if (FJavaLoaderState = GrtLsInitialized) then
  begin
    if (FGrt.Verbose) then
      OutputText(#13#10);

    //Scan for Java plugins
    I := myx_grt_scan_for_modules(FNativeGrt, FGrt.FJavaClasspath + '/com/mysql/grt/modules', @Error);
    OutputModuleStatus('Java', I, Error);

    FJavaLoaderState := GrtLsModulesLoaded;
  end;

  {$ifdef ENABLE_PHP_MODULES}
  // Php modules
  if (FPhpLoaderState = GrtLsInitialized) then
  begin
    //Scan for PHP plugins
    I := myx_grt_scan_for_modules(FNativeGrt, './php/modules', @Error);
    OutputModuleStatus('PHP', I, Error);
  end;
  {$endif}

  // Lua modules
  if (FLuaLoaderState = GrtLsInitialized) and
    (WideDirectoryExists(GetApplDir + 'lua')) then
  begin
    //Scan for Lua plugins
    I := myx_grt_scan_for_modules(FNativeGrt, './lua', @Error);
    OutputModuleStatus('Lua', I, Error);
  end;
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.OutputModuleStatus(LoaderName: WideString; LoadedModuleCount: Integer; Error: MYX_GRT_ERROR);

begin
  if (Error <> MYX_GRT_NO_ERROR) then
    OutputText(Format(
      _('Error while loading %s modules (%d).') + #13#10,
      [LoaderName, Ord(Error)]))
  else
    if (LoadedModuleCount = 1) then
      OutputText(Format(
        _('Registered 1 %s module.') + #13#10, [LoaderName]))
    else
      if (LoadedModuleCount > 0) then
        OutputText(Format(
          _('Registered %d %s modules.') + #13#10, [LoadedModuleCount, LoaderName]))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.TaskFinished;

begin
  FCurrentTask.OnTaskFinished(FCurrentTask);
  FCurrentTask := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtEngine.GetNativeGrt: Pointer;

begin
  Result := FNativeGrt;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.AddTask(Task: IGrtGenericTask);

// Adds the given task to the work queue and returns immediately.
// Once the task was executed its OnTaskFinished callback is called to notify the principal.

begin
  FWorkQueue.Push(Task);
  FWorkEvent.SetEvent;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.AddTaskAndWait(Task: IGrtGenericTask);

// Similar to AddTask, however the method does not return until the task is finished, the given timeout is reached
// or the application is being terminated.

begin
  Task.PrepareSynchronization;
  FWorkQueue.Push(Task);
  FWorkEvent.SetEvent;
  Task.WaitFor(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.AddTaskAndWaitWithMessages(Task: IGrtGenericTask);

// Works like AddTaskAndWait except that the applications can process its messages.
// This is a potential reentrancy problem and therefor this method should only be used by old code.
// Instead use the asynchronous way to wait for a task to finish.

begin
  Task.PrepareSynchronization;
  FWorkQueue.Push(Task);
  FWorkEvent.SetEvent;
  Task.WaitFor(True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.RemoveTask(Task: IGrtGenericTask);

// Removes the given task from the work queue.
// If this task is currently being executed then removing it has no effect.

begin
  FWorkQueue.Remove(Task);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.OutputText(S: WideString);

begin
  AddCallbackEntry(0, S, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.DoOutputText(S: WideString);

begin
  if Assigned(FCurrentTask) and
    (Assigned(FCurrentTask.OnProcessOutput)) then
    FCurrentTask.OnProcessOutput(S)
  else
  begin
    if Assigned(FGrt.FConsole) then
    begin
      FGrt.Console.AddOutput(S);
      FGrt.Console.Invalidate;
    end
    else
      FOutputBuffer := FOutputBuffer + S;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtEngine.ProcessMessages(PMessages: PMYX_GRT_MSGS): Integer;

begin
  AddCallbackEntry(1, '', PMessages);
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.DoProcessMessages(Messages: TMYX_GRT_MSGS);

var
  StandardTask: IGrtTask;

begin
  if Assigned(FCurrentTask) then
  begin
    if not Supports(FCurrentTask, IGrtTask, StandardTask) then
      StandardTask := nil;
    if Assigned(StandardTask) and Assigned(StandardTask.OnProcessMessages) then
      StandardTask.OnProcessMessages(Messages)
    else
    begin
      // print the message
      DoOutputText(FormatGrtMessagesAsString(Messages));
    end;
  end
  else
  begin
    // print the message
    DoOutputText(FormatGrtMessagesAsString(Messages));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtEngine.GrtInputText(Caption: WideString; options: MYX_GRT_INPUT_OPTIONS; Text: PPointer): integer;

var
  Input: string;

begin
  FTextForOutput := Caption;

  // This text is changed in the following input call if there is an input function assigned to the task.
  FTextInput := '@~@';
  Synchronize(DoGrtInputText);

  if FTextInput <> '@~@' then
    Input := UTF8Encode(FTextInput)
  else
  begin
    // Wait till user input is finished
    FGrt.GrtSyncEvent := TEvent.Create(nil, False, False, '');
    try
      FGrt.GrtSyncEvent.WaitFor(10000000);

      Input := UTF8Encode(FGrt.Console.ConsoleCommand);
      Input := Copy(Input, 0, Length(Input) - 2);
    finally
      FGrt.Console.ReadScriptInput := False;
      FGrt.GrtSyncEvent.Free;
      FGrt.GrtSyncEvent := nil;
    end;
  end;

  StrLCopy(FInputBuffer, PChar(Input), 160);

  Text^ := @FInputBuffer;

  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.DoGrtInputText;

var
  StandardTask: IGrtTask;

begin
  StandardTask := nil;
  if Assigned(FCurrentTask) then
  begin
    if not Supports(FCurrentTask, IGrtTask, StandardTask) then
      StandardTask := nil;
  end;
  if Assigned(StandardTask) and Assigned(StandardTask.OnPerformInput) then
    FTextInput := StandardTask.OnPerformInput(FTextForOutput)
  else
  begin
    FGrt.Console.ReadScriptInput := True;
    FGrt.Console.ConsolePrompt := FTextForOutput;
    FGrt.Console.Content.DeleteLine(FGrt.Console.Content.Count - 1);
    FGrt.Console.PrepareNextConsoleCommand;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtEngine.GrtStatusQuery: Integer;

begin
  Synchronize(DoStatusQuery);
  Result := FStatus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.DoStatusQuery;

var
  StandardTask: IGrtTask;

begin
  StandardTask := nil;
  if Assigned(FCurrentTask) then
    if not Supports(FCurrentTask, IGrtTask, StandardTask) then
      StandardTask := nil;
  if Assigned(StandardTask) and Assigned(StandardTask.OnPerformStatusQuery) then
    FStatus := StandardTask.OnPerformStatusQuery
  else
    FStatus := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.ExecuteDelphiFunction;

begin
  FDelphiModuleFunctionResult :=
    FDelphiModuleImplementor.GrtExecuteModuleFunction(
    FDelphiModuleName, FDelphiModuleFunctionName, FDelphiModuleFunctionArgument);
end;

//----------------------------------------------------------------------------------------------------------------------

function DelphiGrtCallFunction(func: PMYX_GRT_FUNCTION; argument: Pointer; retval: PPointer): MYX_GRT_ERROR cdecl;

var
  GrtEngine: TGrtEngine;

begin
  Result := MYX_GRT_NO_ERROR;

  GrtEngine := func.module.priv;

  retval^ := nil;

  try
    GrtEngine.FDelphiModuleImplementor := IGrtModuleImplementor(func.priv);
    GrtEngine.FDelphiModuleName := UTF8Decode(func.module.name);
    GrtEngine.FDelphiModuleFunctionName := UTF8Decode(func.name);
    GrtEngine.FDelphiModuleFunctionArgument := argument;

    // Call function synchronized
    GrtEngine.Synchronize(GrtEngine.ExecuteDelphiFunction);

    GrtEngine.FDelphiModuleImplementor := nil;

    retval^ := GrtEngine.FDelphiModuleFunctionResult;
  except
    on x: Exception do
    begin
      // Log error
      GrtEngine.DelphiGrtMessages.msgs.Add(
        TMYX_GRT_MSG.Create(1, x.Message, -1, nil));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtEngine.InitDelphiLoader(Error: PMYX_GRT_ERROR): Pointer;

begin
  Result := myx_grt_module_loader_create(FNativeGrt,
    MYX_DELPHI_MODULE_TYPE, nil,
    nil, @DelphiGrtCallFunction,
    self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.AddDelphiModule(ModuleName: WideString; ModuleFunctions: WideString;
  Implementor: IGrtModuleImplementor);

var
  PModule: PMYX_GRT_MODULE;
  I: Integer;
  FuncList: TTntStringList;
  Impl: Pointer;

begin
  PModule := myx_grt_module_create(FNativeLoader, ModuleName, '', '', self);

  FuncList := TTntStringList.Create;
  try
    FuncList.Text := ModuleFunctions;

    Impl := Pointer(Implementor);

    for I := 0 to FuncList.Count - 1 do
      myx_grt_module_add_function(PModule, FuncList[I], '', '',
        Impl);

    // Add a reference because a pointer to the interfaces is store in the GRT
    Implementor._AddRef;

    myx_grt_add_module(NativeGrt, PModule);
  finally
    FuncList.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.RemoveDelphiModule(ModuleName: WideString;
  Implementor: IGrtModuleImplementor);

begin
  // Release refcount since the interface ref is now no longer stored in the GRT
  Implementor._Release;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor EGrtError.Create(ErrorNumber: Integer; Description: WideString);

begin
  FErrorNumber := ErrorNumber;
  FDescription := Description;

  if (FErrorNumber <> 0) then
    Message := FDescription + ' Error Nr.' + IntToStr(FErrorNumber)
  else
    Message := FDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetListMemberCount(Grt: Pointer; StructName: WideString; OnlyCheck: Boolean): Integer;

var
  PStruct,
    PMember: Pointer;
  i,
    count: integer;

begin
  PStruct := myx_grt_struct_get(Grt, StructName);

  count := 0;
  for i := 0 to myx_grt_struct_get_member_count_total_excluding_struct(
    Grt, PStruct, 'db.DatabaseObject') - 1 do
  begin
    PMember := myx_grt_struct_get_member_by_index_total(
      Grt, PStruct, i);

    if (myx_grt_struct_member_get_type(PMember) = MYX_LIST_VALUE) and
      (myx_grt_struct_member_get_content_type(PMember) = MYX_DICT_VALUE) then
    begin
      inc(count);

      if (OnlyCheck) then
        break;
    end;
  end;

  Result := count;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetListMember(Grt: Pointer; StructName: WideString; Index: Integer): Pointer;

var
  PStruct: Pointer;
  i,
  Count: integer;

begin
  Result := nil;
  PStruct := myx_grt_struct_get(Grt, StructName);

  count := 0;
  for i := 0 to myx_grt_struct_get_member_count_total(Grt, PStruct) - 1 do
  begin
    Result := myx_grt_struct_get_member_by_index_total(
      Grt, PStruct, i);

    if (myx_grt_struct_member_get_type(Result) = MYX_LIST_VALUE) and
      (myx_grt_struct_member_get_content_type(Result) = MYX_DICT_VALUE) then
    begin
      inc(Count);

      if (Index = Count - 1) then
        break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function FormatGrtMessagesAsString(Msgs: TMYX_GRT_MSGS): WideString;

var
  S: WideString;
  I,
  J: Integer;

begin
  S := '';

  for I := 0 to Msgs.msgs.Count - 1 do
  begin
    // do not log progress messages
    if (Msgs.msgs[I].msg_type = 2) then
      Continue;

    if (Msgs.msgs[I].msg_type = 1) then
      S := S + 'ERROR: ';

    S := S +
      Tnt_WideStringReplace(
      Tnt_WideStringReplace(
      Msgs.msgs[I].msg, #13#10, #10, [rfReplaceAll], False),
      #10, #13#10, [rfReplaceAll], False) +
      #13#10;

    if (Msgs.msgs[I].msg_detail <> nil) then
    begin
      for j := 0 to Msgs.msgs[I].msg_detail.strings.Count - 1 do
      begin
        S := S +
          Tnt_WideStringReplace(
          Tnt_WideStringReplace(
          Msgs.msgs[I].msg_detail.strings[J], #13#10, #10, [rfReplaceAll], False),
          #10, #13#10, [rfReplaceAll], False) +
          #13#10;
      end;
    end;
  end;

  Result := S;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetGrtFunctionArgumentAsString(Argument: Pointer): WideString;

var
  ValueType: MYX_GRT_VALUE_TYPE;
  Value: Pointer;

begin
  Result := '';

  ValueType := myx_grt_value_get_type(Argument);

  if (ValueType = MYX_STRING_VALUE) then
    Result := myx_grt_value_as_string(Argument)
  else
    if (ValueType = MYX_LIST_VALUE) then
    begin
      Value := myx_grt_list_item_get(Argument, 0);
      ValueType := myx_grt_value_get_type(Value);

      if (ValueType = MYX_STRING_VALUE) then
        Result := myx_grt_value_as_string(Value)
    end;

  if (ValueType <> MYX_STRING_VALUE) then
    raise Exception.Create('A wrong argument was passed to the function.');
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtEngine.ApplicationHook(var Message: TMessage): Boolean;

begin
  Result := False;
  case Message.Msg of
    WM_PROCESS_CALLBACK:
      begin
        DoChangeCallback;
        Result := True;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.ClearCallbackEntries;

var
  I: Integer;
  Entry: PCallbackEntry;

begin
  with FCallbackEntries, LockList do
  try
    for I := 0 to Count - 1 do
    begin
      Entry := Items[I];
      Entry.Messages.Free;
      Dispose(Entry);
    end;
    Clear;
  finally
    UnlockList;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtEngine.AddCallbackEntry(AType: Integer; OutputText: WideString; Messages: PMYX_GRT_MSGS);

var
  Entry: PCallbackEntry;

begin
  with FCallbackEntries, LockList do
  try
    New(Entry);
    Entry.Type_ := AType;
    Entry.TextForOutput := OutputText;
    Entry.Messages := TMYX_GRT_MSGS.Create(Messages);
    FCallbackEntries.Add(Entry);
  finally
    UnlockList;
  end;

  PostMessage(Application.Handle, WM_PROCESS_CALLBACK, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TGrtEngine.DoChangeCallback;

var
  Entry: PCallbackEntry;

begin
  repeat
    with FCallbackEntries, LockList do
    begin
      if Count > 0 then
      begin
        Entry := First;
        Remove(Entry);
      end
      else
        Entry := nil;
      UnlockList;
    end;

    if Entry = nil then
      Break;

    case Entry.Type_ of
      0:
        DoOutputText(Entry.TextForOutput);
      1:
       begin
          DoProcessMessages(Entry.Messages);
          Entry.Messages.Free;
       end;
    end;
    Dispose(Entry);
  until False;
end;

//----------------- TGrtTaskList ---------------------------------------------------------------------------------------

constructor TGrtTaskList.Create;

begin
  inherited Create;

  InitializeCriticalSection(FLock);
  FList := TInterfaceList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGrtTaskList.Destroy;

begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTaskList.GetEmpty: Boolean;

begin
  LockList;    // Make sure nobody else is inside the list.
  try
    Result := FList.Count = 0;
  finally
    UnlockList;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtTaskList.SetEmpty(const Value: Boolean);

begin
  if Value then
    Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtTaskList.Clear;

begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTaskList.LockList: TInterfaceList;

begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTaskList.Pop: IGrtGenericTask;

begin
  LockList;
  try
    Result := IGrtGenericTask(FList[0]);
    FList.Delete(0);
  finally
    UnlockList;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtTaskList.Push(Task: IGrtGenericTask);

begin
  LockList;
  try
    FList.Add(Task);
  finally
    UnlockList;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtTaskList.Remove(Task: IGrtGenericTask);

begin
  LockList;
  try
    FList.Remove(Task);
  finally
    UnlockList;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtTaskList.UnlockList;

begin
  LeaveCriticalSection(FLock);
end;

//-----------------´TGrtGenericTask ------------------------------------------------------------------------------------

destructor TGrtGenericTask.Destroy;

begin
  FWaitEvent.Free;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetAllowNullResult: Boolean;

begin
  Result := FAllowNullResult;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetDataObj: TObject;

begin
  Result := FDataObj;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetErrorCode: MYX_GRT_ERROR;

begin
  Result := FErrorCode;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetErrorString: WideString;

begin
  Result := FErrorString;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetExecutionTime: Cardinal;

begin
  Result := FExecutionTime;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetProcessOutputFunction: TGrtProcessOutput;

begin
  Result := FOnProcessOutput;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetTag: Integer;

begin
  Result := FTag;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetTaskFinishedFunction: TGrtTaskFinished;

begin
  Result := FOnTaskFinished;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetTaskID: Integer;

begin
  Result := FTaskID;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtGenericTask.GetTimeout: Integer;

begin
  Result := FTimeout;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtGenericTask.PrepareSynchronization;

// Prepares the task for a following WaitFor call.

begin
  // The creation call is not synchronized against multiple use of the task by different threads.
  // A task should be "owned" only by one thread at a time. However it can be executed multiple times.
  if FWaitEvent = nil then
    FWaitEvent := TEvent.Create(nil, True, False, '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtGenericTask.SetErrorCode(const Value: MYX_GRT_ERROR);

begin
  FErrorCode := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtGenericTask.SetErrorString(const Value: WideString);

begin
  FErrorString := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtGenericTask.SetEvent;

// Signals any waiting thread that this task is finished.

begin
  if Assigned(FWaitEvent) then
    FWaitEvent.SetEvent;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtGenericTask.SetExecutionTime(const Value: Cardinal);

begin
  FExecutionTime := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtGenericTask.SetTag(Tag: Integer);

begin
  FTag := Tag;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtGenericTask.WaitFor(WithMessages: Boolean);

// Waits for the task to be finished by locking the calling thread. The task must have been prepared by a
// PrepareSynchronization call. The timeout value stored in FTimeout is used to limit the wait call to
// a maximum time. If this timeout value is -1, which means to wait forever, then the termination state of the application
// is checked each second and the WaitFor call is ended in case the application is about to be terminated.
// The parameter tells the method whether to process application messages while it waits. This is a potential reentrancy
// problem and therefor this method should only be used by old code. Instead use the asynchronous way to wait for a
// task to finish.

var
  WaitTime: Integer;

begin
  if FTimeout = -1 then
  begin
    if WithMessages then
      WaitTime := 20
    else
      WaitTime := 1000;
    while not Application.Terminated do
      if FWaitEvent.WaitFor(WaitTime) = wrSignaled then
        Break
      else
        if WithMessages then
          Application.ProcessMessages;
  end
  else
    FWaitEvent.WaitFor(FTimeout);
end;

//----------------- TGrtTask -------------------------------------------------------------------------------------------

destructor TGrtTask.Destroy;

begin
  if FFunctionArguments <> nil then
    FGrt.ValueRelease(FFunctionArguments);
    
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetDescription: WideString;

begin
  Result := FDescription;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetFunctionArgument: Pointer;

begin
  Result := FFunctionArguments;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetFunctionName: WideString;

begin
  Result := FFunctionName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetModulName: WideString;

begin
  Result := FModulName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetPerformInputFunction: TGrtPerformInput;

begin
  Result := FOnPerformInput;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetPerformStatusQueryFunction: TGrtPerformStatusQuery;

begin
  Result := FOnPerformStatusQuery;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetProcessMessagesFunction: TGrtProcessMessages;

begin
  Result := FOnProcessMessages;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetResult: Pointer;

begin
  Result := FResult;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtTask.GetSearchParent: Boolean;

begin
  Result := FSearchParent;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtTask.SetResult(const Value: Pointer);

begin
  FResult := Value;
end;

//----------------- TGrtShellTask --------------------------------------------------------------------------------------

function TGrtShellTask.GetCommand: WideString;

begin
  Result := FCommand;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGrtShellTask.GetResult: MYX_GRT_SHELL_COMMAND;

begin
  Result := FResult;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGrtShellTask.SetResult(const Value: MYX_GRT_SHELL_COMMAND);

begin
  FResult := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetGrtClass(AClass: TGrtClass);

begin
  if GrtClass <> AClass then
  begin
    FreeAndNil(InternalGrtInstance);
    GrtClass := AClass;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  SetGrtClass(TGrt);
end.

