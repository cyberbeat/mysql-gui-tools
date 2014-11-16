
#include "myx_grt_public_interface.h"


#if 0
int main()
{
  MYX_GRT_ERROR error;
  MYX_GRT_AGENT_SESSION *session;
  MYX_GRT_AGENT_STATUS status;
  MYX_GRT_VALUE *arg, *result;
  int done= 0;

  arg= myx_grt_dict_create(NULL,
			"username", MYX_STRING_VALUE, "root",
		        "hostname", MYX_STRING_VALUE, "localhost",
		NULL);

   session= myx_grt_remote_connect("localhost", 12345, "foobar");
   if (session)
	g_message("connect ok");
   else
	g_message("connect FAIL");

   if (myx_grt_remote_function_invoke(session, "ReverseEngineeringMysql", "getSchemata", arg) == MYX_GRTA_OK)
	g_message("invoke OK");

   while (!done)
   {
      g_message("checking..");
      switch (myx_grt_remote_function_check(session))
      {
        case MYX_GRTA_FINISHED:
	g_message("done"); 
        done= 1;
        break; 
        case MYX_GRTA_EXECUTING:
	g_message("working...");
        usleep(50000);
        break;
      }
   }

   result= myx_grt_remote_function_finish(session, &error, &status);
   if (status!= MYX_GRTA_FINISHED)
     g_message("FINISH NOT OK");
 //  else
 //    g_message("success (%i): %s", error, myx_grt_value_to_xml(result));
}
#endif
int main()
{
}

