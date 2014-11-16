//---------------------------------------------------------------------------

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/tree.h>
#include <libxml/xmlIO.h>

#include <iostream>
#include <fstream>
#include <list>
#include <set>


//#include <stdio.h>

#pragma hdrstop

class Global_parameters
{
public:
  // operation mode
  enum Operation_mode {
    UNKNOWN,    // unset
    UPDATE_XML, // update the project xml file with the new values from the list file
    DUMP_XML    // dump xml project file into the list file
  };

private:

  const char *list_file_name, *proj_file_name;

  Operation_mode op_mode;

  void print_usage()
  {
    std::cout
         << std::endl
         << "FilelistConvertTool v.1.0 (c) 2006 MySQL AB" << std::endl << std::endl
         << "Usage: FilelistConvertTool.exe --mode=[update|dump] --list-file=<list-file> --proj-file=<proj-file>" << std::endl
         << "    or FilelistConvertTool.exe --help to see this help screen" << std::endl << std::endl
         << "Arguments:" << std::endl
         << "    update - update xml project with data from the list file" << std::endl
         << "    dump   - dump all files fro xml project to the list file" << std::endl;
    exit(1);
  }

public:

  Global_parameters(int argc, char *argv[])
  {
    op_mode= UNKNOWN;
    proj_file_name= NULL;
    list_file_name= NULL;

    // read args
    size_t sz;
    for(int i= 1; i < argc; i++) {
      sz= sizeof("--help")-1;
      if(strncmp("--help", argv[i], sz) == 0) {
        print_usage();  // this function exists program
        continue;
      }
      sz= sizeof("--mode=update")-1;
      if(strncmp("--mode=update", argv[i], sz) == 0) {
        op_mode= UPDATE_XML;
        continue;
      }
      sz= sizeof("--mode=dump")-1;
      if(strncmp("--mode=dump", argv[i], sz) == 0) {
        op_mode= DUMP_XML;
        continue;
      }
      sz= sizeof("--list-file=")-1;
      if(strncmp("--list-file=", argv[i], sz) == 0) {
        list_file_name= strcpy(new char[strlen(argv[i]) - sz+1], argv[i] + sz);
        continue;
      }
      sz= sizeof("--proj-file=")-1;
      if(strncmp("--proj-file=", argv[i], sz) == 0) {
        proj_file_name= strcpy(new char[strlen(argv[i]) - sz+1], argv[i] + sz);
        continue;
      }
    }

    if((proj_file_name == NULL) || (list_file_name == NULL) || (op_mode == UNKNOWN))
    {
      print_usage();
    }
  }

  ~Global_parameters()
  {
    if(list_file_name)
      delete[] list_file_name;
    if(proj_file_name)
      delete[] proj_file_name;
  }

  const char *get_list_file_name() const
  {
    return list_file_name;
  }

  const char *get_proj_file_name() const
  {
    return proj_file_name;
  }

  Operation_mode get_operation_mode() const
  {
    return op_mode;
  }
};

void make_text_file_backup(const char *filename)
{
  std::string backup_name(filename);
  backup_name.append(".bak");

  std::ifstream is(filename);
  std::ofstream os(backup_name.c_str());

  const size_t sz= 65536;
  char *buf= new char[sz];

  while(!is.eof())
  {
    is.read(buf, sz);
    if(is.rdstate() != std::ios::goodbit)
    {
      break;
    }
    os << buf;
  }

  delete[] buf;
}

//---------------------------------------------------------------------------

// xpath used to retrieve source file list
const char *cc_xpath=
  "/BorlandProject/CPlusPlusBuilder.Personality/BCBPROJECT/FILELIST/"
  "FILE[@CONTAINERID = 'CCompiler']/@FILENAME";

// xpath used to delete all source files
const char *del_xpath=
  "/BorlandProject/CPlusPlusBuilder.Personality/BCBPROJECT/FILELIST/"
  "FILE[@CONTAINERID = 'CCompiler']";

// xpath used to find parent node to add source files to
const char *add_xpath=
  "/BorlandProject/CPlusPlusBuilder.Personality/BCBPROJECT/FILELIST";

void get_filenames_from_xml_file(const char *filename, std::list<char *> *filenames)
{
  xmlDocPtr doc= xmlParseFile(filename);

  if(doc == NULL)
  {
    std::cerr << "Error parsing XML document " << filename << std::endl;
    exit(1);
  }

  xmlXPathContextPtr context= xmlXPathNewContext(doc);
  xmlXPathObjectPtr result= xmlXPathEval((xmlChar*)cc_xpath, context);
  xmlNodeSetPtr node_set= result->nodesetval;

  for(size_t i= 0; node_set->nodeTab[i] != NULL; i++)
  {
    xmlNode *cur_node = node_set->nodeTab[i];
    char *content= reinterpret_cast<char *>(cur_node->children->content);
    filenames->push_back(strcpy(
        new char[strlen(content) + 1], content));
  }
}

void delete_filenames_from_xml(xmlDocPtr doc)
{
  xmlXPathContextPtr context= xmlXPathNewContext(doc);
  xmlXPathObjectPtr result= xmlXPathEval((xmlChar*)del_xpath, context);
  xmlNodeSetPtr node_set= result->nodesetval;

  for(size_t i= 0; node_set->nodeTab[i] != NULL; i++)
  {
    xmlNode *cur_node = node_set->nodeTab[i];
    xmlUnlinkNode(cur_node);
    xmlFreeNode(cur_node);
  }
}

void add_filenames_to_xml(xmlDocPtr doc, std::list<char *> *filenames)
{
  xmlXPathContextPtr context= xmlXPathNewContext(doc);
  xmlXPathObjectPtr result= xmlXPathEval((xmlChar*)add_xpath, context);
  xmlNodeSetPtr node_set= result->nodesetval;

  xmlNode *parent_node = node_set->nodeTab[0];

  std::list<char *>::const_iterator e= filenames->end();
  for(std::list<char *>::const_iterator it= filenames->begin(); it != e; it++)
  {
    xmlNode *node_file= xmlNewChild(parent_node, NULL, (xmlChar*)"FILE", (xmlChar*)"");
    xmlNewProp(node_file, (xmlChar*)"FILENAME", (xmlChar*)*it);
    xmlNewProp(node_file, (xmlChar*)"CONTAINERID", (xmlChar*)"CCompiler");
    xmlNewProp(node_file, (xmlChar*)"LOCALCOMMAND", (xmlChar*)"");
    xmlNewProp(node_file, (xmlChar*)"UNITNAME", (xmlChar*)"");
    xmlNewProp(node_file, (xmlChar*)"FORMNAME", (xmlChar*)"");
    xmlNewProp(node_file, (xmlChar*)"DESIGNCLASS", (xmlChar*)"");
  }
}

// SOURCES=path/file1.c path/file2.c path/file3.c path/file4.c 

void get_filenames_from_list_file(const char *filename, std::list<char *> *filenames)
{
  std::ifstream is(filename);

  std::string word;
  char c;

  while(!is.eof() && (c != '='))
  {
    c= is.get();
    word.append(1, c);
  }

#ifdef __GNUC__
  if(strcasecmp(word.c_str(), "SOURCES=") != 0)
#else
  if(stricmp(word.c_str(), "SOURCES=") != 0)
#endif
  {
    std::cerr << "Error parsing list document " << filename << std::endl;
    exit(1);
  }

  word.clear();

  while(!is.eof())
  {
    c= is.get();
    if(c == ' ')
    {
      filenames->push_back(strcpy(new char[word.length() + 1], word.c_str()));
      word.clear();
    }
    else if(!is.eof())
    {
      word.append(1, c);
    }
  }

  if(word.length() > 0)
  {
    filenames->push_back(strcpy(new char[word.length() + 1], word.c_str()));
  }
}

void dump_xml(const Global_parameters *params)
{
  make_text_file_backup(params->get_list_file_name());

  std::ofstream os(params->get_list_file_name());
  os << "SOURCES=";

  std::list<char *> filenames;
  get_filenames_from_xml_file(params->get_proj_file_name(), &filenames);

  std::list<char *>::const_iterator e= filenames.end();
  for(std::list<char *>::const_iterator it= filenames.begin(); it != e; it++)
  {
    os << *it << " ";
    delete[] *it;
  }
  filenames.clear();
}

/*
struct char_ptr_less : public std::binary_function<char *, char *, bool> {
  bool operator()(const char * left, const char * right) const
  {
    while(*left && *right)
    {
      if(*left < *right)
      {
        return true;
      }
      if(*left > *right)
      {
        return false;
      }
      ++left;
      ++right;
    }
    if(*left || !*right)  // if left is longer or they're equal
    {
      return false;
    }
    return true;
  }
};
*/

void update_xml(const Global_parameters *params)
{
  xmlDocPtr doc= xmlParseFile(params->get_proj_file_name());

  if(doc == NULL)
  {
    std::cerr << "Error parsing XML document " << params->get_proj_file_name() << std::endl;
    exit(1);
  }

  make_text_file_backup(params->get_proj_file_name());

  delete_filenames_from_xml(doc);

  std::list<char *>filenames;
  get_filenames_from_list_file(params->get_list_file_name(), &filenames);

  add_filenames_to_xml(doc, &filenames);

  char *doc_dump;
  int size;

  xmlDocDumpFormatMemory(doc, (xmlChar **)&doc_dump, &size, 1);
  std::ofstream os(params->get_proj_file_name());
  os << doc_dump;
  xmlFree(doc_dump);

  std::list<char *>::const_iterator e= filenames.end();
  for(std::list<char *>::const_iterator it= filenames.begin(); it != e; it++)
  {
    delete[] *it;
  }
}

//---------------------------------------------------------------------------

// <FILE
//  FILENAME="test-source\main.cpp"
//  CONTAINERID="CCompiler"
//  LOCALCOMMAND=""
//  UNITNAME="main"
//  FORMNAME=""
//  DESIGNCLASS="" />

#pragma argsused
int main(int argc, char* argv[])
{
  Global_parameters params(argc, argv);

  xmlInitParser();

  if(params.get_operation_mode() == Global_parameters::DUMP_XML)
  {
    dump_xml(&params);
  }
  else
  {
    update_xml(&params);
  }

  xmlCleanupParser();

  return 0;
}
//---------------------------------------------------------------------------
