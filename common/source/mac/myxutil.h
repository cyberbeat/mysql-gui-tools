
#ifndef __MYXUTIL_H__
#define __MYXUTIL_H__

#import <Foundation/Foundation.h>
#include "myx_public_interface.h"

MYX_USER_CONNECTION *myx_copy_user_connection(MYX_USER_CONNECTION *info);

NSString *myx_get_available_filename(NSString *directory, NSString *base, NSString *suffix);

int myx_same_user_connections(MYX_USER_CONNECTION *conn1,
                              MYX_USER_CONNECTION *conn2);


const char *myx_get_option_value(const char **list, unsigned int count, const char *option);

#define NSStr(s) [NSString stringWithUTF8String:(s)?:""]
#define NSInt(s) [NSNumber numberWithInt:s]

#endif
