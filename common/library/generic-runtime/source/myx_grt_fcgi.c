/* Copyright (c) 2005 MySQL AB
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
 */

#include "myx_grt_fcgi.h"

static void fcgi_process_output(const char *text, void *user_data)
{
  char *output= str_g_replace(g_strdup(text), _br, "<br>");

  FCGX_FPrintF(((FCGX_Request *)user_data)->out, output);

  g_free(output);
}

static void fcgi_print_404(FCGX_Request *request)
{
  FCGX_FPrintF(request->out,
    "Content-type: text/html\r\n"
      "\r\n"
      "<html><head><title>MySQL GRT Shell</title></head>\r\n"
      "<body><h2>Error 404 - File not found</h2>\r\n"
      "%s\r\n", FCGX_GetParam("SCRIPT_NAME", request->envp));
}

static void fcgi_print_status(FCGX_Request *request)
{
  char *content_length;
  int len = 0;
  char **envp= request->envp;

  FCGX_FPrintF(request->out,
      "Content-type: text/html\r\n"
      "\r\n"
      "<html><head><title>MySQL GRT Shell</title></head>"
      "<body><h2>MySQL GRT Shell FastCGI</h2>");

  content_length = FCGX_GetParam("CONTENT_LENGTH", request->envp);
  if (content_length != NULL)
    len = strtol(content_length, NULL, 10);

  if (len <= 0) 
  {
    FCGX_FPrintF(request->out, "No data from standard input.<p>\n");
  } 
  else 
  {
    int i, ch;

    FCGX_FPrintF(request->out, "Standard input:<br>\n<pre>\n");
    for (i = 0; i < len; i++) {
      if ((ch = FCGX_GetChar(request->in)) < 0) {
        FCGX_FPrintF(request->out,
            "Error: Not enough bytes received on standard input<p>\n");
        break;
      }
      FCGX_PutChar(ch, request->out);
    }
    FCGX_FPrintF(request->out, "\n</pre><p>\n");
  }

  FCGX_FPrintF(request->out, "FCGI Environment Variables:<br>\n<pre>\n");
  for( ; *envp != NULL; envp++) {
    FCGX_FPrintF(request->out, "%s\n", *envp);
  }
  FCGX_FPrintF(request->out, "</pre><p>\n");
}


void myx_grt_start_fcgi(MYX_GRT *grt, int port, char **allowed_modules, 
                   unsigned int allowed_modules_num)
{
  FCGX_Request request;
  int sock;
  char port_string[7];

  if (FCGX_Init()) 
  {
    g_message("Init failed.\n");
    return;
  }

  g_snprintf(port_string, 7, ":%i", port);

  sock= FCGX_OpenSocket(port_string, 50);
  if (sock < 0)
  {
    g_message("Error trying to open socket %i. (%d).\n", port, sock);
    return;
  }

  g_message("Start listening at port %i.\n", port);

  FCGX_InitRequest(&request, sock, 0);

  for (;;)
  {
    int rc = FCGX_Accept_r(&request);    

    if (rc < 0)
        break;

    myx_grt_set_output_callback(grt, &request, &fcgi_process_output);

    if (strcmp2(FCGX_GetParam("SCRIPT_NAME", request.envp), "/info.lsp") == 0)
    {
      fcgi_print_status(&request);
    }
    else if (strcmp2(FCGX_GetParam("SCRIPT_NAME", request.envp), "/shell.lsp") == 0)
    {
      char cmd_buffer[256];
      int len;

      FCGX_FPrintF(request.out,
        "Content-type: text/html\r\n"
        "\r\n"
        "<html>\r\n"
        "<head>\r\n"
        "  <title>MySQL GRT Shell</title>\r\n"
        "  <style type=\"text/css\">\r\n"
        "  <!--\r\n"
        "  * { background-color: #000000; color:#888888; font-size: small;\r\n"
        "      font-family: \"Bitstream Vera Sans Mono\",Courier New,Courier; font-weight: bold} \r\n"
        "  input { border-width: 0; border-style: none; } \r\n"
        "  .output { margin-left: 16px } \r\n"
        "  -->\r\n"
        "  </style>\r\n"
        "</head>\r\n"
        "<body onLoad=\"javascript: document.f.cmd.focus();\" >\r\n"
        "<div>");
        

      len= FCGX_GetStr(cmd_buffer, 255, request.in);
      if (len > 0)
      {
        char cmd[256];

        cmd_buffer[len]= 0;

        value_of_str(cmd, cmd_buffer);

        FCGX_FPrintF(request.out,
          "> %s</div>"
          "<div class=\"output\">",
          cmd);

        myx_grt_shell_execute(grt, cmd);
      }

      FCGX_FPrintF(request.out,
        "</div>\r\n"
        "<form name=\"f\" action=\"/shell.lsp\" method=\"post\" enctype=\"text/plain\">\r\n"
        "&gt;&nbsp;<input name=\"cmd\" type=\"text\" size=\"80\">\r\n"
        "</form>\r\n");
    }
    else
    {
      fcgi_print_404(&request);
    }

    myx_grt_set_output_callback(grt, NULL, NULL);

    FCGX_FPrintF(request.out, "</body></html>");

    FCGX_Finish_r(&request);
  }

  g_message("Exiting.\n");

  return;
}

