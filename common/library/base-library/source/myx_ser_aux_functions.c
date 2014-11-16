/* Copyright (C) 2004 MySQL AB

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

#include "myx_ser_aux_functions.h"
#include <glib.h>
#include <string.h>
#include <stdio.h>
#include <sys/socket.h>

struct _MYX_SFD
{
  int fd;
  char *in_buffer;
  size_t in_buffer_used;
  size_t in_buffer_size;
};

int ser_string(MYX_SFD *fd, const char *s)
{
  return send(fd->fd, s, strlen(s)+1, 0);
}

int ser_int(MYX_SFD *fd, int value)
{
  char buffer[32];
  g_snprintf(buffer, sizeof(buffer), "%i", value);
  return send(fd->fd, buffer, strlen(buffer)+1, 0);
}


int unser_string(MYX_SFD *fd, char **s)
{
  char *ptr;
  int rc;

  if (!fd->in_buffer)
  {
    fd->in_buffer_size= 1024;
    fd->in_buffer_used= 0;
    fd->in_buffer= g_malloc(fd->in_buffer_size);
  }

  while (fd->in_buffer_used==0 || (ptr= memchr(fd->in_buffer, 0, fd->in_buffer_used))==NULL)
  {
    if (fd->in_buffer_used > fd->in_buffer_size - 128)
    {
      fd->in_buffer_size+= 1024;
      fd->in_buffer= g_realloc(fd->in_buffer, fd->in_buffer_size);
    }
    
    rc= recv(fd->fd, fd->in_buffer+fd->in_buffer_used, fd->in_buffer_size-fd->in_buffer_used, 0);
    if (rc <= 0)
      return rc;
    fd->in_buffer_used+= rc;
  }
  ptr++;

  *s= g_strdup(fd->in_buffer);
  memmove(fd->in_buffer, ptr,
          fd->in_buffer_used-(ptr-fd->in_buffer));
  fd->in_buffer_used-= (ptr-fd->in_buffer);

  return ptr-fd->in_buffer;
}


int unser_int(MYX_SFD *fd, int *value)
{
  char *tmp;
  int rc;
  
  rc= unser_string(fd, &tmp);
  if (rc > 0)
  {
    sscanf(tmp, "%i", value);
    g_free(tmp);
  }
  return rc;
}



MYX_SFD *myx_sfd_from_fd(int fd)
{
  MYX_SFD *sfd= g_malloc0(sizeof(MYX_SFD));
  sfd->fd= fd;
  return sfd;
}


int myx_free_sfd(MYX_SFD *sfd)
{
  if (sfd->in_buffer)
    g_free(sfd->in_buffer);
  g_free(sfd);
  return 0;
}
