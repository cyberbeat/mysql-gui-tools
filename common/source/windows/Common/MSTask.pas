unit MSTask;

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
  Windows, ActiveX;
  
//----------------------------------------------------------------------------------------------------------------------
//
//  Task Scheduler definitions and interfaces.
//
//  Microsoft Windows
//  Copyright (C) Microsoft Corporation, 1992 - 1999.
//
//  In MSVC this file is automatically created from IDL.
//
// Compiler settings for mstask.idl:
//    Oicf, W1, Zp8, env=Win32 (32b run)
//    protocol : dce , ms_ext, c_ext, robust
//    error checks: allocation ref bounds_check enum stub_data
//    VC __declspec() decoration level:
//         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
//         DECLSPEC_UUID(), MIDL_INTERFACE()
//
//----------------------------------------------------------------------------------------------------------------------

{$MINENUMSIZE 4}

const
  {$EXTERNALSYM TASK_SUNDAY}
  TASK_SUNDAY      = $1;
  {$EXTERNALSYM TASK_MONDAY}
  TASK_MONDAY      = $2;
  {$EXTERNALSYM TASK_TUESDAY}
  TASK_TUESDAY     = $4;
  {$EXTERNALSYM TASK_WEDNESDAY}
  TASK_WEDNESDAY   = $8;
  {$EXTERNALSYM TASK_THURSDAY}
  TASK_THURSDAY    = $10;
  {$EXTERNALSYM TASK_FRIDAY}
  TASK_FRIDAY      = $20;
  {$EXTERNALSYM TASK_SATURDAY}
  TASK_SATURDAY    = $40;
  {$EXTERNALSYM TASK_FIRST_WEEK}
  TASK_FIRST_WEEK  = 1;
  {$EXTERNALSYM TASK_SECOND_WEEK}
  TASK_SECOND_WEEK = 2;
  {$EXTERNALSYM TASK_THIRD_WEEK}
  TASK_THIRD_WEEK  = 3;
  {$EXTERNALSYM TASK_FOURTH_WEEK}
  TASK_FOURTH_WEEK = 4;
  {$EXTERNALSYM TASK_LAST_WEEK}
  TASK_LAST_WEEK   = 5;
  {$EXTERNALSYM TASK_JANUARY}
  TASK_JANUARY     = $1;
  {$EXTERNALSYM TASK_FEBRUARY}
  TASK_FEBRUARY    = $2;
  {$EXTERNALSYM TASK_MARCH}
  TASK_MARCH       = $4;
  {$EXTERNALSYM TASK_APRIL}
  TASK_APRIL       = $8;
  {$EXTERNALSYM TASK_MAY}
  TASK_MAY         = $10;
  {$EXTERNALSYM TASK_JUNE}
  TASK_JUNE        = $20;
  {$EXTERNALSYM TASK_JULY}
  TASK_JULY        = $40;
  {$EXTERNALSYM TASK_AUGUST}
  TASK_AUGUST      = $80;
  {$EXTERNALSYM TASK_SEPTEMBER}
  TASK_SEPTEMBER   = $100;
  {$EXTERNALSYM TASK_OCTOBER}
  TASK_OCTOBER     = $200;
  {$EXTERNALSYM TASK_NOVEMBER}
  TASK_NOVEMBER    = $400;
  {$EXTERNALSYM TASK_DECEMBER}
  TASK_DECEMBER    = $800;

  {$EXTERNALSYM TASK_FLAG_INTERACTIVE}
  TASK_FLAG_INTERACTIVE                  = $1;

  {$EXTERNALSYM TASK_FLAG_DELETE_WHEN_DONE}
  TASK_FLAG_DELETE_WHEN_DONE             = $2;

  {$EXTERNALSYM TASK_FLAG_DISABLED}
  TASK_FLAG_DISABLED                     = $4;

  {$EXTERNALSYM TASK_FLAG_START_ONLY_IF_IDLE}
  TASK_FLAG_START_ONLY_IF_IDLE           = $10;

  {$EXTERNALSYM TASK_FLAG_KILL_ON_IDLE_END}
  TASK_FLAG_KILL_ON_IDLE_END             = $20;

  {$EXTERNALSYM TASK_FLAG_DONT_START_IF_ON_BATTERIES}
  TASK_FLAG_DONT_START_IF_ON_BATTERIES   = $40;

  {$EXTERNALSYM TASK_FLAG_KILL_IF_GOING_ON_BATTERIES}
  TASK_FLAG_KILL_IF_GOING_ON_BATTERIES   = $80;

  {$EXTERNALSYM TASK_FLAG_RUN_ONLY_IF_DOCKED}
  TASK_FLAG_RUN_ONLY_IF_DOCKED           = $100;

  {$EXTERNALSYM TASK_FLAG_HIDDEN}
  TASK_FLAG_HIDDEN                       = $200;

  {$EXTERNALSYM TASK_FLAG_RUN_IF_CONNECTED_TO_INTERNET}
  TASK_FLAG_RUN_IF_CONNECTED_TO_INTERNET = $400;

  {$EXTERNALSYM TASK_FLAG_RESTART_ON_IDLE_RESUME}
  TASK_FLAG_RESTART_ON_IDLE_RESUME       = $800;

  {$EXTERNALSYM TASK_FLAG_SYSTEM_REQUIRED}
  TASK_FLAG_SYSTEM_REQUIRED              = $1000;

  {$EXTERNALSYM TASK_FLAG_RUN_ONLY_IF_LOGGED_ON}
  TASK_FLAG_RUN_ONLY_IF_LOGGED_ON        = $2000;

  {$EXTERNALSYM TASK_TRIGGER_FLAG_HAS_END_DATE}
  TASK_TRIGGER_FLAG_HAS_END_DATE         = $1;

  {$EXTERNALSYM TASK_TRIGGER_FLAG_KILL_AT_DURATION_END}
  TASK_TRIGGER_FLAG_KILL_AT_DURATION_END = $2;

  {$EXTERNALSYM TASK_TRIGGER_FLAG_DISABLED}
  TASK_TRIGGER_FLAG_DISABLED             = $4;

const
  {$EXTERNALSYM TASK_MAX_RUN_TIMES}
  TASK_MAX_RUN_TIMES = 1440;

type
  {$EXTERNALSYM _TASK_TRIGGER_TYPE}
  _TASK_TRIGGER_TYPE = (
    {$EXTERNALSYM TASK_TIME_TRIGGER_ONCE}
    TASK_TIME_TRIGGER_ONCE = 0,                       
    {$EXTERNALSYM TASK_TIME_TRIGGER_DAILY}
    TASK_TIME_TRIGGER_DAILY,                          
    {$EXTERNALSYM TASK_TIME_TRIGGER_WEEKLY}
    TASK_TIME_TRIGGER_WEEKLY,                         
    {$EXTERNALSYM TASK_TIME_TRIGGER_MONTHLYDATE}
    TASK_TIME_TRIGGER_MONTHLYDATE,                    
    {$EXTERNALSYM TASK_TIME_TRIGGER_MONTHLYDOW}
    TASK_TIME_TRIGGER_MONTHLYDOW,                     
    {$EXTERNALSYM TASK_EVENT_TRIGGER_ON_IDLE}
    TASK_EVENT_TRIGGER_ON_IDLE,                       
    {$EXTERNALSYM TASK_EVENT_TRIGGER_AT_SYSTEMSTART}
    TASK_EVENT_TRIGGER_AT_SYSTEMSTART,                
    {$EXTERNALSYM TASK_EVENT_TRIGGER_AT_LOGON}
    TASK_EVENT_TRIGGER_AT_LOGON                       
  );

  {$EXTERNALSYM TASK_TRIGGER_TYPE}
  TASK_TRIGGER_TYPE = _TASK_TRIGGER_TYPE;

  {$EXTERNALSYM PTASK_TRIGGER_TYPE}
  PTASK_TRIGGER_TYPE = ^_TASK_TRIGGER_TYPE;
  TTaskTriggerType = _TASK_TRIGGER_TYPE;
  PTaskTriggerType = PTASK_TRIGGER_TYPE;

  {$EXTERNALSYM DAILY}
  DAILY = packed record
    DaysInterval: Word;
  end;
  TDaily = DAILY;

  {$EXTERNALSYM WEEKLY}
  WEEKLY = packed record
     WeeksInterval: Word;
     rgfDaysOfTheWeek: Word;
  end;
  TWeekly = WEEKLY;

  {$EXTERNALSYM MONTHLYDATE}
  MONTHLYDATE = packed record
    rgfDays: DWORD;
    rgfMonths: Word;
    fill: Word; // This member is not in the original source, but MONTHLYDATE is 8 bytes in size in MSVC.
  end;
  TMonthlyDate = MONTHLYDATE;

  {$EXTERNALSYM MONTHLYDOW}
  MONTHLYDOW = packed record
    wWhichWeek: Word;
    rgfDaysOfTheWeek: Word;
    rgfMonths: Word;
  end;
  TMonthlyDOW = MONTHLYDOW;

  {$EXTERNALSYM TRIGGER_TYPE_UNION}
  TRIGGER_TYPE_UNION = packed record
    case Integer of
      0: (Daily: DAILY);
      1: (Weekly: WEEKLY);
      2: (MonthlyDate: MONTHLYDATE);
      3: (MonthlyDOW: MONTHLYDOW);
  end;
  TTriggerTypeUnion = TRIGGER_TYPE_UNION;

  {$EXTERNALSYM TASK_TRIGGER}
  TASK_TRIGGER = packed record
    cbTriggerSize: Word;
    Reserved1: Word;                 
    wBeginYear: Word;                
    wBeginMonth: Word;               
    wBeginDay: Word;                 
    wEndYear: Word;                  
    wEndMonth: Word;                 
    wEndDay: Word;                   
    wStartHour: Word;                
    wStartMinute: Word;              
    MinutesDuration: DWORD;          
    MinutesInterval: DWORD;          
    rgFlags: DWORD;                  
    TriggerType: TASK_TRIGGER_TYPE;  
    Type_: TRIGGER_TYPE_UNION;       
    Reserved2: Word;                 
    wRandomMinutesInterval: Word;    
  end;
  {$EXTERNALSYM PTASK_TRIGGER}
  PTASK_TRIGGER = ^TASK_TRIGGER;
  TTaskTrigger = TASK_TRIGGER;
  PTaskTrigger = PTASK_TRIGGER;

const
  {$EXTERNALSYM IID_ITaskTrigger}
  IID_ITaskTrigger: TIID = '{148BD52B-A2AB-11CE-B11F-00AA00530503}';

  {$EXTERNALSYM IID_IScheduledWorkItem}
  IID_IScheduledWorkItem: TIID = '{a6b952f0-a4b1-11d0-997d-00aa006887ec}';

  {$EXTERNALSYM IID_ITask}
  IID_ITask: TIID = '{148BD524-A2AB-11CE-B11F-00AA00530503}';

  {$EXTERNALSYM IID_IEnumWorkItems}
  IID_IEnumWorkItems: TIID = '{148BD528-A2AB-11CE-B11F-00AA00530503}';

  {$EXTERNALSYM IID_ITaskScheduler}
  IID_ITaskScheduler: TIID = '{148BD527-A2AB-11CE-B11F-00AA00530503}';

  {$EXTERNALSYM CLSID_CTask}
  CLSID_CTask: TCLSID = '{148BD520-A2AB-11CE-B11F-00AA00530503}';

  {$EXTERNALSYM CLSID_CTaskScheduler}
  CLSID_CTaskScheduler: TCLSID = '{148BD52A-A2AB-11CE-B11F-00AA00530503}';

  {$EXTERNALSYM IID_IProvideTaskPage}
  IID_IProvideTaskPage: TIID = '{4086658a-cbbb-11cf-b604-00c04fd8d565}';

  {$EXTERNALSYM IID_ISchedulingAgent}
  IID_ISchedulingAgent: TIID = '{148BD527-A2AB-11CE-B11F-00AA00530503}';

  {$EXTERNALSYM CLSID_CSchedulingAgent}
  CLSID_CSchedulingAgent: TCLSID = '{148BD52A-A2AB-11CE-B11F-00AA00530503}';

type
  {$EXTERNALSYM ITaskTrigger}
  ITaskTrigger = interface(IUnknown)
    ['{148BD52B-A2AB-11CE-B11F-00AA00530503}']
    function SetTrigger(const pTrigger: TTaskTrigger): HRESULT; stdcall;
    function GetTrigger(out pTrigger: TTaskTrigger): HRESULT; stdcall;
    function GetTriggerString(out ppwszTrigger: LPWSTR): HRESULT; stdcall;
  end;

  {$EXTERNALSYM IScheduledWorkItem}
  IScheduledWorkItem = interface(IUnknown)
    ['{a6b952f0-a4b1-11d0-997d-00aa006887ec}']
    function CreateTrigger(out piNewTrigger: Word; out ppTrigger: ITaskTrigger): HRESULT; stdcall;
    function DeleteTrigger(iTrigger: Word): HRESULT; stdcall;
    function GetTriggerCount(out pwCount: Word): HRESULT; stdcall;
    function GetTrigger(iTrigger: Word; out ppTrigger: ITaskTrigger): HRESULT; stdcall;
    function GetTriggerString(iTrigger: Word; out ppwszTrigger: LPWSTR): HRESULT; stdcall;
    function GetRunTimes(pstBegin, pstEnd: PSystemTime; var pCount: Word; out rgstTaskTimes: PSystemTime): HRESULT; stdcall;
    function GetNextRunTime(var pstNextRun: SYSTEMTIME): HRESULT; stdcall;
    function SetIdleWait(wIdleMinutes, wDeadlineMinutes: Word): HRESULT; stdcall;
    function GetIdleWait(out pwIdleMinutes, pwDeadlineMinutes: Word): HRESULT; stdcall;
    function Run: HRESULT; stdcall;
    function Terminate: HRESULT; stdcall;
    function EditWorkItem(hParent: HWND; dwReserved: DWORD): HRESULT; stdcall;
    function GetMostRecentRunTime(out pstLastRun: SYSTEMTIME): HRESULT; stdcall;
    function GetStatus(out phrStatus: HRESULT): HRESULT; stdcall;
    function GetExitCode(out pdwExitCode: DWORD): HRESULT; stdcall;
    function SetComment(pwszComment: LPCWSTR): HRESULT; stdcall;
    function GetComment(out ppwszComment: LPWSTR): HRESULT; stdcall;
    function SetCreator(pwszCreator: LPCWSTR): HRESULT; stdcall;
    function GetCreator(out ppwszCreator: LPWSTR): HRESULT; stdcall;
    function SetWorkItemData(cbData: Word; rgbData: PByte): HRESULT; stdcall;
    function GetWorkItemData(out pcbData: Word; out prgbData: PByte): HRESULT; stdcall;
    function SetErrorRetryCount(wRetryCount: Word): HRESULT; stdcall;
    function GetErrorRetryCount(out pwRetryCount: Word): HRESULT; stdcall;
    function SetErrorRetryInterval(wRetryInterval: Word): HRESULT; stdcall;
    function GetErrorRetryInterval(out pwRetryInterval: Word): HRESULT; stdcall;
    function SetFlags(dwFlags: DWORD): HRESULT; stdcall;
    function GetFlags(out pdwFlags: DWORD): HRESULT; stdcall;
    function SetAccountInformation(pwszAccountName, pwszPassword: LPCWSTR): HRESULT; stdcall;
    function GetAccountInformation(out ppwszAccountName: LPWSTR): HRESULT; stdcall;
  end;

  {$EXTERNALSYM ITask}
  ITask = interface(IScheduledWorkItem)
    ['{148BD524-A2AB-11CE-B11F-00AA00530503}']
    function SetApplicationName(pwszApplicationName: LPCWSTR): HRESULT; stdcall;
    function GetApplicationName(out ppwszApplicationName: LPWSTR): HRESULT; stdcall;
    function SetParameters(pwszParameters: LPCWSTR): HRESULT; stdcall;
    function GetParameters(out ppwszParameters: LPWSTR): HRESULT; stdcall;
    function SetWorkingDirectory(pwszWorkingDirectory: LPCWSTR): HRESULT; stdcall;
    function GetWorkingDirectory(out ppwszWorkingDirectory: LPWSTR): HRESULT; stdcall;
    function SetPriority(dwPriority: DWORD): HRESULT; stdcall;
    function GetPriority(out pdwPriority: DWORD): HRESULT; stdcall;
    function SetTaskFlags(dwFlags: DWORD): HRESULT; stdcall;
    function GetTaskFlags(out pdwFlags: DWORD): HRESULT; stdcall;
    function SetMaxRunTime(dwMaxRunTimeMS: DWORD): HRESULT; stdcall;
    function GetMaxRunTime(out pdwMaxRunTimeMS: DWORD): HRESULT; stdcall;
  end;

  {$EXTERNALSYM IEnumWorkItems}
  IEnumWorkItems = interface(IUnknown)
    ['{148BD528-A2AB-11CE-B11F-00AA00530503}']
    function Next(celt: ULONG; out rgpwszNames: PLPWSTR; out pceltFetched: ULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnumWorkItems: IEnumWorkItems): HRESULT; stdcall;
  end;

  {$EXTERNALSYM ITaskScheduler}
  ITaskScheduler = interface(IUnknown)
    ['{148BD527-A2AB-11CE-B11F-00AA00530503}']
    function SetTargetComputer(pwszComputer: LPCWSTR): HRESULT; stdcall;
    function GetTargetComputer(out ppwszComputer: LPWSTR): HRESULT; stdcall;
    function Enum(out ppEnumWorkItems: IEnumWorkItems): HRESULT; stdcall;
    function Activate(pwszName: LPCWSTR; const riid: TIID; out ppUnk: IUnknown): HRESULT; stdcall;
    function Delete(pwszName: LPCWSTR): HRESULT; stdcall;
    function NewWorkItem(pwszTaskName: LPCWSTR; const rclsid: TCLSID; const riid: TIID; out ppUnk: IUnknown): HRESULT; stdcall;
    function AddWorkItem(pwszTaskName: LPCWSTR; pWorkItem: IScheduledWorkItem): HRESULT; stdcall;
    function IsOfType(pwszName: LPCWSTR; const riid: TIID): HRESULT; stdcall;
  end;

type
  {$EXTERNALSYM _PSP}
  _PSP = record end;
  {$EXTERNALSYM HPROPSHEETPAGE}
  HPROPSHEETPAGE = ^_PSP;

  {$EXTERNALSYM _TASKPAGE}
  _TASKPAGE = (
    {$EXTERNALSYM TASKPAGE_TASK}
    TASKPAGE_TASK,
    {$EXTERNALSYM TASKPAGE_SCHEDULE}
    TASKPAGE_SCHEDULE,
    {$EXTERNALSYM TASKPAGE_SETTINGS}
    TASKPAGE_SETTINGS
  );
  {$EXTERNALSYM TASKPAGE}
  TASKPAGE = _TASKPAGE;


  {$EXTERNALSYM IProvideTaskPage}
  IProvideTaskPage = interface(IUnknown)
    ['{4086658a-cbbb-11cf-b604-00c04fd8d565}']
    function GetPage(tpType: TASKPAGE; fPersistChanges: BOOL; out phPage: HPROPSHEETPAGE): HRESULT; stdcall;
  end;

  {$EXTERNALSYM ISchedulingAgent}
  ISchedulingAgent = ITaskScheduler;

  {$EXTERNALSYM IEnumTasks}
  IEnumTasks = IEnumWorkItems;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
