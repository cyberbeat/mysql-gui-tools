//
//  MGRTModuleLoader.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/8/23.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include <MySQLGRT/myx_grt_public_interface.h>

@interface MGRTModuleLoader : NSObject {
  MYX_GRT *_grt;
}

- (id)initWithGRT:(MYX_GRT*)grt;
- (MYX_GRT_MODULE_LOADER*)initLoader;

// anObject must respond to -(BOOL)exportsMethod:(NSString*)name
// which should return YES if the named method should be exported in the module
//
// the exported methods should be like - (MYX_GRT_VALUE*)method:(NSArray*)args
- (MYX_GRT_MODULE*)registerObject:(id)anObject
                         asModule:(NSString*)moduleName;

@end
