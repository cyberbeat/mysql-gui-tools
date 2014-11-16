#include    "FTPixmapGlyph.h"

FTPixmapGlyph::FTPixmapGlyph(FT_GlyphSlot glyph, bool useList)
:   FTGlyph(glyph, useList),
    destWidth(0),
    destHeight(0),
    data(0),
    glList(0)
{
    err = FT_Render_Glyph( glyph, FT_RENDER_MODE_NORMAL);
    if( err || ft_glyph_format_bitmap != glyph->format)
    {
        return;
    }

    FT_Bitmap bitmap = glyph->bitmap;

    int srcWidth = bitmap.width;
    int srcHeight = bitmap.rows;
    
    destWidth = srcWidth;
    destHeight = srcHeight;
    
    if( destWidth && destHeight)
    {
      data = new unsigned char[destWidth * destHeight * 4];
      unsigned char* src = bitmap.buffer;

      unsigned char* dest = data + ((destHeight - 1) * destWidth * 4);
      size_t destStep = destWidth * 4 * 2;

      for( int y = 0; y < srcHeight; ++y)
      {
        for( int x = 0; x < srcWidth; ++x)
        {
          *dest++ = 255;
          *dest++ = 255;
          *dest++ = 255;
          *dest++ = *src++;
        }
        dest -= destStep;
      };
      destHeight = srcHeight;
    };
                                    
    pos.X(glyph->bitmap_left);
    pos.Y(srcHeight - glyph->bitmap_top);

    if (useList)
    {
      glList = glGenLists(1);
      glNewList(glList, GL_COMPILE);
      glPixelStorei( GL_UNPACK_ROW_LENGTH, 0);
      glPixelStorei( GL_UNPACK_ALIGNMENT, 4);
      glDrawPixels(destWidth, destHeight, GL_RGBA, GL_UNSIGNED_BYTE, data);
      glEndList();

      // If used in a display list the data is transferred to the graphics board, so we can save some memory.
      delete data;
      data = NULL;
    };
}


FTPixmapGlyph::~FTPixmapGlyph()
{
    delete [] data;
}


const FTPoint& FTPixmapGlyph::Render( const FTPoint& pen)
{
  glBitmap( 0, 0, 0.0f, 0.0f, float (pen.X() + pos.X()), float(pen.Y() - pos.Y()), NULL);

  if (glList != 0)
    glCallList(glList);
  else
    if( data)
    {
      glPixelStorei( GL_UNPACK_ROW_LENGTH, 0);
      glPixelStorei( GL_UNPACK_ALIGNMENT, 4);
      glDrawPixels(destWidth, destHeight, GL_RGBA, GL_UNSIGNED_BYTE, data);
    }
        
  glBitmap( 0, 0, 0.0f, 0.0f, (float) -pos.X(), (float) pos.Y(), NULL);

  return advance;
}
