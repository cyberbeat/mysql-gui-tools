unit ColorTypes;

//----------------------------------------------------------------------------------------------------------------------
// ColorTools contains general purpose functions for handling colors and requires Delphi 6 or better to compile.
// The calculations contained here are optimized for precision, not for speed, while still keeping complexity at a
// reasonable level. Unless otherwise specified all XYZ coordinates are given for a 2° observer.
// For color management an additional unit is used (LCMS, Little Color Management System by Marti Maria (www.littlecms.com).
//
// This software is public domain. You may freely use it in any software, including commercial software, provided
// this header remains unchanged.
//
// Although it is not required it would be a nice move to recognize my work by adding a citation to the application's
// about box or a similar place.
//
// The original code is ColorTypes.pas, released October 1, 2004.
//
// The initial developer of the original code is:
//   Mike Lischke, Delphi Gems software solutions (support@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2004 Delphi Gems. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

type
  // Format neutral storage of a color. Can be anything.
  TColorComponents = array of Double;

  // RGB (red, green, blue) color given in the range [0..1] per component.
  TRGB = record
    R, G, B: Double;
  end;

  // RGB color with alpha given in the range [0..1] per component.
  TRGBA = record
    R, G, B, A: Double;
  end;

  // CMYK (cyan, magenta, yellow, black) color given in the range [0..1] per component.
  TCMYK = record
    C, M, Y, K: Double;
  end;

  // Hue, luminance, saturation color with all three components in the range [0..1]
  // (so hue's 0..360° is normalized to 0..1).
  THLS = record
    H, L, S: Double;
  end;

  // CIE L*a*b* color with L in [0..100], a and b in [-127..128].
  TLab = record
    L, a, b: Double;
  end;

  PCIELab = ^TCIELab;
  TCIELab = TLab;
  
  // CIE XYZ color model, which is an abstract and deviced-independent system to convert between different
  // other color models (e.g. from RGB to CIE L*a*b*).
  TXYZ = record
    X, Y, Z: Double;
  end;

  TCIEXYZ = TXYZ;
  PCIEXYZ = ^TCIEXYZ;

  TxyY = record
    x, y, YY: Double;
  end;

  TCIExyY = TxyY;
  PCIExyY = ^TCIExyY;
  
  TLCh = record
    L, C, h: Double
  end;

  TCIELCh = TLCh;
  PCIELCh = ^TCIELCh;
  
  PJCh  = ^TJCh;
  TJCh   = record
    J, C, h: Double
  end;

  // Transformation matrix for linear conversions between color schemes (e.g. CIE XYZ to RGB) and for general
  // color manipulations.
  TColorMatrix = array[0..2, 0..2] of Double;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
