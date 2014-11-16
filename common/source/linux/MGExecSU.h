

#ifndef _MGEXECSU_H_
#define _MGEXECSU_H_

#include "MGGladeXML.h"

class MGExecSU {
 public:
   enum Status {
     RUNFINISHED=-1,
     RSUCCESS=0,
     RBADPASSWORD,
     RTIMEOUT,
     RERROR,
   };
   
   typedef sigc::signal<bool,Glib::ustring> OutputSignal;
   typedef sigc::signal<void,Status> FinishedSignal;

 private:
   MGGladeXML *_xml;
   Glib::ustring _command;
   pid_t _pid;
   FILE *_rfd;
   FILE *_wfd;
   Status _status;
   int _timeout_seconds;
   int _timeout_counter;
   bool _needs_su;
   
   OutputSignal _output_signal;
   FinishedSignal _finished_signal;

   
   bool wait_readable(int timeout= 0);
   bool read_until(const char **tokens, Glib::ustring &line);
   
   bool handle_io(Glib::IOCondition cond);
   
   bool write_line(const Glib::ustring &line);

   bool timeout();
   
   bool authenticate();
   int check_needs_password();
   
   bool run_as_root();
   bool run_as_current();

 public:
   MGExecSU(const Glib::ustring &description, const Glib::ustring &command);

   void set_timeout(int seconds);
   
   bool run();
   void cancel();

   bool read_line(Glib::ustring &line, bool full_line= true);
   Glib::ustring read_to_end();
   
   OutputSignal signal_output() { return _output_signal; };
   FinishedSignal signal_finished() { return _finished_signal; };

   
   Status get_status() { return _status; };
};


#endif /* _MGEXECSU_H_ */
