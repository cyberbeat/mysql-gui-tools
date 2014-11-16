/* Copyright (C) 2003 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include <myx_library.h>
#include <myx_network.h>

// Windows includes
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <windows.h>
//#include <winsock2.h>
// Include icmp headers for ping
#include <ipexport.h>
#include <icmpapi.h>
#else

#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/socket.h>

#endif

/** @defgroup Host_information_management_private internal stuff
 *  @ingroup Host_information_management */

/** @addtogroup Host_information_management_private
 *  @{ */

/*
 * functions
 */

///////////////////////////////////////////////////////////////////////////////
/** @brief retrieve the host information for internet address
    @param address internet addres 
                    (ip string like {'127','0','0','1'} or dns name)
    @return information about host
*//////////////////////////////////////////////////////////////////////////////
struct hostent *get_host_info(const char *address)
{
   /* get host information by host's IP number */
   if (isdigit(address[0]))
   {
      unsigned long hostnum = inet_addr(address);
      return gethostbyaddr ((char *)&hostnum, sizeof (unsigned long), AF_INET);
   }
   /* get host information by host's computer name */
   else
   {
      return gethostbyname ((char *) address);
   }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get first ip for hostname as unsigned long
    @ingroup Host_information_management
    @param hostname name of host
    @return first ip for hostname as unsigned long
*//////////////////////////////////////////////////////////////////////////////
unsigned long myx_resolve_network_name(const char *hostname)
{
  struct hostent* phostent= gethostbyname(hostname);

  if (phostent == NULL)
    return 0;

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  return *(DWORD*)(*phostent->h_addr_list);
#else
  return *(unsigned long*)(*phostent->h_addr_list);
#endif
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get first ip for hostname as string
    @param hostname name of host
    @param ip       char buffer to return ip.
                    strlen(ip) should be equal or greater
                    than strlen("xxx.xxx.xxx.xxx"))
    @return function returns Zero always.
*//////////////////////////////////////////////////////////////////////////////
int myx_get_ip_as_string(const char *hostname, char *ip)
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  struct in_addr in;
#endif
  struct hostent *phostent;

  phostent= gethostbyname(hostname);

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  if (phostent)
  {
    in.S_un.S_un_b.s_b1= phostent->h_addr_list[0][0];
    in.S_un.S_un_b.s_b2= phostent->h_addr_list[0][1];
    in.S_un.S_un_b.s_b3= phostent->h_addr_list[0][2];
    in.S_un.S_un_b.s_b4= phostent->h_addr_list[0][3];

    sprintf(ip, "%s", inet_ntoa(in));
  }
  else
    strcpy(ip, "");
#else
  if (phostent)
    strcpy(ip, inet_ntoa(*(struct in_addr*)phostent->h_addr_list[0]));
  else
    strcpy(ip, "");
#endif

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief convert ip to string
    @param phostent pointer to hostent structure with information about host
    @param ip_index index of ip addres of host in the phostend->h_addr_list
    @param ip char buffer to print to
*//////////////////////////////////////////////////////////////////////////////
void ip_to_str(struct hostent *phostent, unsigned int ip_index, char *ip)
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  struct in_addr in;
  in.S_un.S_un_b.s_b1= phostent->h_addr_list[ip_index][0];
  in.S_un.S_un_b.s_b2= phostent->h_addr_list[ip_index][1];
  in.S_un.S_un_b.s_b3= phostent->h_addr_list[ip_index][2];
  in.S_un.S_un_b.s_b4= phostent->h_addr_list[ip_index][3];
  sprintf(ip, "%s", inet_ntoa(in));
#else
  strcpy(ip, inet_ntoa(*(struct in_addr*)phostent->h_addr_list[0]));
#endif
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get network name by ip name

    @param ip       internet addres 
                     (ip string like {'127','0','0','1'} or dns name)
    @param hostname char buffer for returned dns name of host.
                     strlen(hostname) should be equal or great 64

    @return Zero if network name was retrieved succesefully else -1
*//////////////////////////////////////////////////////////////////////////////
int myx_get_network_name_by_ip(const char *ip, char *hostname)
{
  struct hostent *hostptr;

  hostptr= get_host_info(ip);
  if (hostptr == NULL)
  {
    hostname[0]= 0;
    return -1;
  }
  
  strncpy(hostname, hostptr->h_name, NETWORK_NAME_BUFFER_LENGTH-1);
  hostname[NETWORK_NAME_BUFFER_LENGTH-1]= 0;

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get network name by ip name
    @ingroup Host_information_management

    @param ip           ip address to ping
    @param ping_timeout maximum time to wait for reply
    @param ping_result  buffer to return result of ping

    @return vva_todo.. (it looks like Zero if ping was successeful)
*//////////////////////////////////////////////////////////////////////////////
int myx_ping_host(unsigned long ip, int ping_timeout,
                  MYX_PING_RESULT *ping_result)
{
  int res= -1;
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  char reply[sizeof(struct icmp_echo_reply)+8];
  struct icmp_echo_reply* iep=(struct icmp_echo_reply*)&reply;

  HANDLE icmphandle = IcmpCreateFile();
    iep->RoundTripTime = 0xffffffff;
    if (IcmpSendEcho(icmphandle, ip, 0, 0, NULL, reply, 
                    sizeof(struct icmp_echo_reply)+8, ping_timeout))
    {
      ping_result->round_trip_time= iep->RoundTripTime;
      ping_result->ttl= iep->Options.Ttl;
      res= 0;
    }
    if (iep->Status > 0)
      res= iep->Status-11000+1;
  IcmpCloseHandle(icmphandle);
#else
  char buf[32];
  char result[128];
  pid_t pid;
  FILE *f;
  char *argv[8];
  
  strcpy(buf, (char*)inet_ntoa(*(struct in_addr*)&ip));
  
  argv[0]= "/bin/ping";
  argv[1]= buf;
  argv[2]= "-c";
  argv[3]= "1";
  argv[4]= NULL;

  /*
   * exec() external /bin/ping since doing it ourselves would require
   * the program being ran as root.
   */
  
  ping_result->round_trip_time= 0xffffffff;

  f= myx_popen(argv, &pid);
  if (f) 
  {
    while (1)
    {
      if (myx_read_timeout(f, ping_timeout, result, sizeof(result)) > 0)
      {
        float rtime;
        int ttl;

        char *p= strstr(result, "ttl=");
        if (p && sscanf(p, "ttl=%i time=%f", &ttl, &rtime) == 2) 
        {
          ping_result->round_trip_time= rtime;
          ping_result->ttl= ttl;
          res= 0;
          break;
        }
      } 
      else 
      {
        break;
      }
    }
    myx_pclose(f, pid);
  }
#endif
  return res;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief check if hostname is a name of localhost
    @ingroup Host_information_management
    @param hostname checked name
    @return Zero if the hostname is a name of the localhost else Non-zero
*//////////////////////////////////////////////////////////////////////////////
int myx_is_localhost(const char *hostname)
{
  char ip[IP_BUFFER_LENGTH];
  char ip2[IP_BUFFER_LENGTH];
  char network_name[NETWORK_NAME_BUFFER_LENGTH+1];

  struct hostent * phostent;
  int i;

  //Get host IP as string
  myx_get_ip_as_string(hostname, ip);

  if (!strncmp(ip, "127.0.0.1", sizeof("127.0.0.1")-1))
    return 1;

  //Get localhost IPs
  gethostname(network_name, NETWORK_NAME_BUFFER_LENGTH);
  phostent= gethostbyname(network_name);

  if (phostent)
  {
    for (i= 0; phostent->h_addr_list[i]; i++)
    {
      ip_to_str(phostent, i, ip2);
      if (strcmp(ip, ip2)==0)
        return 1;
    }
  }

  return 0;
}

/** @} */ // end of Host_information_management_private
