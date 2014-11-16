//---------------------------------------------------------------------------

#include <windows.h>
#include <mysql.h>

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
{
  switch (reason)
  {
    case DLL_PROCESS_ATTACH:
      break;

    case DLL_THREAD_ATTACH:
      mysql_thread_init();
      break;

    case DLL_THREAD_DETACH:
      mysql_thread_end();
      break;

    case DLL_PROCESS_DETACH:
      // Perform any necessary cleanup.
      break;
  };

  return 1;
}

//---------------------------------------------------------------------------
