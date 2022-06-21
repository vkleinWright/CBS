#
#                              --- version 4.2 ----
#                              - supports Jenkins -
#
#-------------------------------------------------------------------------------------------
# --- Libraries ----------------------------------------------- Edit for this Project ------
#-------------------------------------------------------------------------------------------

# library for programs  
BINLIB=WSCCBS

# library for data 
FILELIB=WSCCBS

# library for CNX
CNXLIB=VALENCE52P

# Other libraries you need for rpg compiles (in biblical order - the last will be first)
# Your BINLIB and FILELIB will be added to the end
INIT_LIBLIST = CMSFIL WSCFIL WSCLIB

# Libraries you will need for UAT for rpg compiles (in biblical order - the last will be first)
# If this build is not at production level, it will be added between the intial libraries and
# your BINLIB and FILELIB will be added to the end
UAT_LIBLIST :=  WSCFIL2 WSCLIB2 WSCFIL2

# other repositories your code might need - in the order you would expect
# Note: Utility is standard and should always be at the end
REPOLIST :=  Utility 



#-------------------------------------------------------------------------------------------
# --- Standard variable setup -------------------------------------- Do Not Change ---------
#-------------------------------------------------------------------------------------------


# shell to use (for consistency)
SHELL=/QOpenSys/usr/bin/qsh

# Compile option for easy debugging
DBGVIEW=*SOURCE

REPO_TEXT= '$(BUILD_TAG)'
SQL_LOC = $(BUILD_TAG)




#--------------------------------------------------------------------
# Fill variable BASELIBS - it will be used to add these to the library list
# Note: If your base libraries are not WSCFIL and WSCLIB, you will probably want to  
#       hard code them in your liblist below.
ifeq ($(FILELIB), $(BINLIB))
    BASELIBS:=$(FILELIB)
else
    BASELIBS:=$(FILELIB) $(BINLIB)
endif


#--------------------------------------------------------------------
# set the switch for searchpath

ifeq ($(GIT_BRANCH),origin/dev)
    STAGE=DEV
else ifeq ($(GIT_BRANCH),origin/uat)
    STAGE=UAT
else 
    STAGE=MAIN
endif



#--------------------------------------------------------------------
# set the developer libraries if needed

# get your user name in all caps
USER_UPPER := $(shell echo $(USER) | tr a-z A-Z)

ifeq ($(strip $(GIT_BRANCH)),)

  # If your user name is in the path, we're assuming this is not 
  # going to build in the main libraries
  ifeq ($(USER_UPPER), $(findstring $(USER_UPPER),$(CURDIR)))
  # so override with the BINLIB and FILELIB in binlib.inc in your home directory
      include  ~/binlib.inc
	  
	  # re-set the switch for searchpath
      STAGE=DEVELOPER
	
  # and fill variable ADDLIBS with the overridden values to add these to the library list
     ifeq ($(FILELIB), $(BINLIB))
        ADDLIBS:=$(FILELIB)
     else
        ADDLIBS:=$(FILELIB) $(BINLIB)
     endif
  # and put the path in the text
  REPO_TEXT:= '$(shell pwd)'
  SQL_LOC =  $(subst /,\\,$(shell pwd))
  endif

endif

#--------------------------------------------------------------------
# add the override libraries to the library list
ifneq ($(strip $(OVRFILE)$(OVRBIN)),)
    ifneq ($(strip $(OVRFILE)),)
        FILELIB=$(OVRFILE)
    endif

    ifneq ($(strip $(OVRBIN)),)
        BINLIB=$(OVRBIN)
    endif
    ifeq ($(OVRFILE), $(OVRBIN))
        ADDLIBS:=$(OVRFILE)
     else
        ADDLIBS:=$(OVRFILE) $(OVRBIN)
     endif
endif
#--------------------------------------------------------------------

# Finalize the library list
# Making allowances for USRWRT on test and USRWRT400 on production

SYS := $(shell hostname)

ifeq ($(strip $(SYS)),)
  ifeq ($(findstring ecmstest,$(BUILD_URL)), ecmstest)
      SYS:=WTSBLADE.WRIGHTTREE.COM
  endif
endif

ifeq ($(STAGE),MAIN)
    UAT_LIBLIST=''
endif

ifeq ($(SYS),WTSBLADE.WRIGHTTREE.COM) 
   EXP_LIBLIST = $(subst WRT400,WRT,$(INIT_LIBLIST)) $(CNXLIB) $(subst WRT400,WRT,$(BASELIBS)) $(UAT_LIBLIST)  $(subst WRT400,WRT,$(ADDLIBS))
else 
   EXP_LIBLIST = $(INIT_LIBLIST) $(CNXLIB) $(BASELIBS) $(UAT_LIBLIST) $(ADDLIBS)   
endif

LIBLIST = $(shell echo $(EXP_LIBLIST)| awk '{for (i=NF;i>1;i--) if (!a[$$i]++) printf("%s%s",$$i,FS)}{printf("\n")}'| awk '{ for (i=NF; i>1; i--) printf("%s ",$$i); print $$1; }') 

 
#path for source
VPATH = source:header

#--------------------------------------------------------------------
# build the repository search path to be used in RPG compiles
SEARCHPATH = ''$(CURDIR)'' 

WORKSPACE = /home/WSCOWNER/.jenkins/workspace

# If we are working in our own library
ifeq ($(STAGE),DEVELOPER)
    SEARCHPATH += $(foreach repo,$(REPOLIST),  ''$(dir $(CURDIR))$(repo)'' ''$(WORKSPACE)/$(repo)-uat'' ''$(WORKSPACE)/$(repo)-main'' ''/wright-service-corp/$(repo)'' )
	
#If we are working in Jenkins dev
else ifeq ($(STAGE),DEV)
        SEARCHPATH += $(foreach repo,$(REPOLIST), ''$(WORKSPACE)/$(repo)-dev'' ''$(WORKSPACE)/$(repo)-uat'' ''$(WORKSPACE)/$(repo)-main'' ''/wright-service-corp/$(repo)'' )
		
#If we are working in Jenkins uat
else ifeq ($(STAGE),UAT)
        SEARCHPATH += $(foreach repo,$(REPOLIST), ''$(WORKSPACE)/$(repo)-uat'' ''$(WORKSPACE)/$(repo)-main'' ''/wright-service-corp/$(repo)'' )
	
else
    #If we are working Jenkins main or wright-service-corp
        SEARCHPATH += $(foreach repo,$(REPOLIST), ''$(WORKSPACE)/$(repo)-main'' ''/wright-service-corp/$(repo)'' )
endif
#--------------------------------------------------------------------




#-------------------------------------------------------------------------------------------
# --- Project Specific ---------------------------------------- Edit for this Project ------
#-------------------------------------------------------------------------------------------

# list of objects for your binding directory (format: pgmname_BNDDIRLIST)
gentable_BNDDIRLIST = logerrors.entrysrv gentable.entrymod gentblmod.entrymod



# everything you want to build here
all: tables programs

tables: tabledef.sqlobj 

programs: gentable.pgm

# dependency lists
gentable.pgm: gentable.bnddir gentblmod.sqlrpgmod gentable.lvl2mod
 





#-------------------------------------------------------------------------------------------
# --- Standard Build Rules ------------------------------------- Do Not Change -------------
#-------------------------------------------------------------------------------------------


%.bnddir:
	-system -q "CRTBNDDIR BNDDIR($(BINLIB)/$*) TEXT($(REPO_TEXT))"
	@touch $*.bnddir


# sql statements should build in the data library
%.sqlobj: %.sql
	sed 's/FILELIB/$(FILELIB)/g' ./source/$*.sql  > ./source/$*.sql1
	sed 's/SQL_LOC/$(SQL_LOC)/g' ./source/$*.sql1 > ./source/$*.sql2
	sed 's/SOURCE_DOC/$*/g' ./source/$*.sql2 > ./source/$*.sql3
	liblist -a $(LIBLIST);\
	system "ADDRPYLE SEQNBR(1500) MSGID(CPA32B2) RPY('I')";\
	system "CHGJOB INQMSGRPY(*SYSRPYL)";\
	system "RUNSQLSTM SRCSTMF('./source/$*.sql3')";
	@touch $@
	system "RMVRPYLE SEQNBR(1500)";
	rm ./source/$*.sql1;
	rm ./source/$*.sql2;
	rm ./source/$*.sql3;

# sql statements for temporal tables should build in the data library
%.sqltemp: %.sql
	sed 's/FILELIB/$(FILELIB)/g' ./source/$*.sql  > ./source/$*.sql1
	sed 's/SQL_LOC/$(SQL_LOC)/g' ./source/$*.sql1 > ./source/$*.sql2
	sed 's/SOURCE_DOC/$*/g' ./source/$*.sql2 > ./source/$*.sql3
	liblist -a $(LIBLIST);\
	system "ADDRPYLE SEQNBR(1500) MSGID(CPA32B2) RPY('I')";\
	system "CHGJOB INQMSGRPY(*SYSRPYL)";\
	system "RUNSQL 'ALTER TABLE $(FILELIB)/$* DROP VERSIONING'";\
	system "RUNSQLSTM SRCSTMF('./source/$*.sql3')";
	@touch $@
	system "RMVRPYLE SEQNBR(1500)";
	rm ./source/$*.sql1;
	rm ./source/$*.sql2;
	rm ./source/$*.sql3;

%.sqlrpgmod: %.sqlrpgle
	liblist -a $(LIBLIST);\
	system "CRTSQLRPGI OBJ($(BINLIB)/$*) SRCSTMF('./source/$*.sqlrpgle') \
	COMMIT(*NONE) OBJTYPE(*MODULE) OPTION(*EVENTF) REPLACE(*YES) DBGVIEW($(DBGVIEW)) \
	TEXT($(REPO_TEXT)) \
	compileopt('INCDIR($(SEARCHPATH))')" 
	@touch $@


%.sqlrpgpgm:
	liblist -a $(LIBLIST);\
	system "CRTSQLRPGI OBJ($(BINLIB)/$*) SRCSTMF('./source/$*.sqlrpgle') \
	COMMIT(*NONE) OBJTYPE(*PGM) OPTION(*EVENTF) REPLACE(*YES) DBGVIEW($(DBGVIEW)) \
	TEXT($(REPO_TEXT)) \
	compileopt('INCDIR( $(SEARCHPATH))')"; 
	@touch $@


%.lvl1mod: %.sqlrpgle
	liblist -a $(LIBLIST);\
	system "CRTSQLRPGI OBJ($(BINLIB)/$*) SRCSTMF('./source/$*.sqlrpgle') \
	COMMIT(*NONE) OBJTYPE(*MODULE) OPTION(*EVENTF) REPLACE(*YES) DBGVIEW($(DBGVIEW)) \
	RPGPPOPT(*LVL1) \
	TEXT($(REPO_TEXT)) \
	compileopt('INCDIR( $(SEARCHPATH))')";
	@touch $@


%.lvl1pgm:
	liblist -a $(LIBLIST);\
	system "CRTSQLRPGI OBJ($(BINLIB)/$*) SRCSTMF('./source/$*.sqlrpgle') \
	COMMIT(*NONE) OBJTYPE(*PGM) OPTION(*EVENTF) REPLACE(*YES) DBGVIEW($(DBGVIEW)) \
	RPGPPOPT(*LVL1) \
	TEXT($(REPO_TEXT)) \
	compileopt('INCDIR( $(SEARCHPATH))')";
	@touch $@


%.lvl2mod: %.sqlrpgle
	liblist -a $(LIBLIST);\
	system "CRTSQLRPGI OBJ($(BINLIB)/$*) SRCSTMF('./source/$*.sqlrpgle') \
	COMMIT(*NONE) OBJTYPE(*MODULE) OPTION(*EVENTF) REPLACE(*YES) DBGVIEW($(DBGVIEW)) \
	RPGPPOPT(*LVL2) \
	TEXT($(REPO_TEXT)) \
	compileopt('INCDIR( $(SEARCHPATH))')";
	@touch $@


%.lvl2pgm:
	liblist -a $(LIBLIST);\
	system "CRTSQLRPGI OBJ($(BINLIB)/$*) SRCSTMF('./source/$*.sqlrpgle') \
	COMMIT(*NONE) OBJTYPE(*PGM) OPTION(*EVENTF) REPLACE(*YES) DBGVIEW($(DBGVIEW)) \
	RPGPPOPT(*LVL2) \
	TEXT($(REPO_TEXT)) \
	compileopt('INCDIR( $(SEARCHPATH))')";
	@touch $@


%.rpglemod: %.rpgle
	liblist -a $(LIBLIST);\
	system "CRTRPGMOD MODULE($(BINLIB)/$*) SRCSTMF('./source/$*.rpgle') DBGVIEW($(DBGVIEW)) \
	REPLACE(*YES) TEXT($(REPO_TEXT))" 
	@touch $@


%.rpglepgm: %.rpgle
	liblist -a $(LIBLIST);\
	system "CRTBNDRPG PGM($(BINLIB)/$*) SRCSTMF('./source/$*.rpgle') \
	OPTION(*EVENTF) DBGVIEW($(DBGVIEW)) REPLACE(*YES) TEXT($(REPO_TEXT)) \
	INCDIR($(subst '',',$(SEARCHPATH)))";
	@touch $@


%.pgm:
	-system -q "ADDBNDDIRE BNDDIR($(BINLIB)/$*) OBJ($(patsubst %.entrysrv,(*LIBL/% *SRVPGM *IMMED), $(patsubst %.entrymod,(*LIBL/% *MODULE *IMMED),$($*_BNDDIRLIST))))";
	liblist -a $(LIBLIST);\
	system "CRTPGM PGM($(BINLIB)/$*)  BNDDIR($(BINLIB)/$*) REPLACE(*YES) TEXT($(REPO_TEXT)) ACTGRP(*CALLER)"
	@touch $@


%.cllebndpgm:  %.clle
	-system -q "CRTSRCPF FILE($(BINLIB)/QCLLESRC) RCDLEN(92)"
	system "CPYFRMSTMF FROMSTMF('./source/$*.clle') TOMBR('/QSYS.lib/$(BINLIB).lib/QCLLESRC.file/$*.mbr') MBROPT(*replace)"
	system "CHGPFM FILE($(BINLIB)/QCLLESRC) MBR($*) SRCTYPE(CLLE)"
	liblist -a $(LIBLIST);\
	system "CRTBNDCL PGM($(BINLIB)/$*) SRCFILE($(BINLIB)/QCLLESRC) TEXT($(REPO_TEXT)) DBGVIEW($(DBGVIEW))"
	@touch $@


%.cllemod: %.clle
	-system -q "CRTSRCPF FILE($(BINLIB)/QCLLESRC) RCDLEN(92)"
	system "CPYFRMSTMF FROMSTMF('./source/$*.clle') TOMBR('/QSYS.lib/$(BINLIB).lib/QCLLESRC.file/$*.mbr') MBROPT(*replace)"
	system "CHGPFM FILE($(BINLIB)/QCLLESRC) MBR($*) SRCTYPE(CLLE)"
	liblist -a $(LIBLIST);\
	system "CRTCLMOD MODULE($(BINLIB)/$*) SRCFILE($(BINLIB)/QCLLESRC) SRCMBR($*) OPTION(*EVENTF) REPLACE(*YES) \
	DBGVIEW($(DBGVIEW)) TEXT($(REPO_TEXT))"
	@touch $@


%.dspfile: %.dspf
	-system -q "CRTSRCPF FILE($(BINLIB)/QDDSSRC) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('./source/$*.dspf') TOMBR('/QSYS.lib/$(BINLIB).lib/QDDSSRC.file/$*.mbr') MBROPT(*replace)"
	system "CHGPFM FILE($(BINLIB)/QDDSSRC) MBR($*) SRCTYPE(DSPF)"
	liblist -a $(LIBLIST);\
	system "CRTDSPF FILE($(BINLIB)/$*) SRCFILE($(BINLIB)/QDDSSRC) SRCMBR($*) TEXT($(REPO_TEXT))"
	@touch $@
	

%.prtfile: %.prtf
	-system -q "CRTSRCPF FILE($(BINLIB)/QDDSSRC) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('./source/$*.prtf') TOMBR('/QSYS.lib/$(BINLIB).lib/QDDSSRC.file/$*.mbr') MBROPT(*replace)"
	system "CHGPFM FILE($(BINLIB)/QDDSSRC) MBR($*) SRCTYPE(PRTF)"
	liblist -a $(LIBLIST);\
	system "CRTPRTF FILE($(BINLIB)/$*) SRCFILE($(BINLIB)/QDDSSRC) SRCMBR($*) TEXT($(REPO_TEXT))"
	@touch $@


%.srvpgm:
    # We need the binder source as a member! Also requires a bindir SRCSTMF on CRTSRVPGM not available on all releases.
	-system -q "CRTSRCPF FILE($(BINLIB)/QSRC) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('./header/$*.bndsrc') TOMBR('/QSYS.lib/$(BINLIB).lib/QSRC.file/$*.mbr') MBROPT(*replace)"
	system "CHGPFM FILE($(BINLIB)/QSRC) MBR($*) SRCTYPE(BND)"
	-system -q "ADDBNDDIRE BNDDIR($(BINLIB)/$*) OBJ($(patsubst %.entrysrv,(*LIBL/% *SRVPGM *IMMED), $(patsubst %.entrymod,(*LIBL/% *MODULE *IMMED),$($*_BNDDIRLIST))))";\
	liblist -a $(LIBLIST);\
	system "CRTSRVPGM SRVPGM($(BINLIB)/$*) BNDDIR($(BINLIB)/$*) SRCFILE($(BINLIB)/QSRC) TEXT($(REPO_TEXT))"
	@touch $@


%.pffile: %.pf
	-system -q "CRTSRCPF FILE($(BINLIB)/QDDSSRC) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('./source/$*.pf') TOMBR('/QSYS.lib/$(BINLIB).lib/QDDSSRC.file/$*.mbr') MBROPT(*replace)"
	system "CHGPFM FILE($(BINLIB)/QDDSSRC) MBR($*) SRCTYPE(PF)"
	liblist -a $(LIBLIST);\
	system "CRTPF FILE($(BINLIB)/$*) SRCFILE($(BINLIB)/QDDSSRC) SRCMBR($*) LVLCHK(*NO) TEXT($(REPO_TEXT))"
	@touch $@


%.lgcfile: %.lf
	-system -q "CRTSRCPF FILE($(BINLIB)/QDDSSRC) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('./source/$*.lf') TOMBR('/QSYS.lib/$(BINLIB).lib/QDDSSRC.file/$*.mbr') MBROPT(*replace)"
	system "CHGPFM FILE($(BINLIB)/QDDSSRC) MBR($*) SRCTYPE(LF)"
	liblist -a $(LIBLIST);\
	system "CRTLF FILE($(BINLIB)/$*) SRCFILE($(BINLIB)/QDDSSRC) SRCMBR($*) LVLCHK(*NO) TEXT($(REPO_TEXT))"
	@touch $@


%.entry:
    # Basically do nothing..
	@echo ""
	
%.entrymod:
    # Basically do nothing..
	@echo ""
	
%.entrysrv:
    # Basically do nothing..
	@echo ""
	
%.sqlrpgle:
    # Basically do nothing..
	@echo ""
	