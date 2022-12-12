namespace xercesc_2_5 {
class XMLDeleter {
  ~XMLDeleter();
};
XMLDeleter::~XMLDeleter() {}
class SchemaInfo {
  SchemaInfo(unsigned short, int, int, int, int, unsigned, unsigned short *,
             const unsigned short *, const int *, int *);
  int fBlockDefault;
  int fFinalDefault;
  int fTargetNSURI;
  int fScopeCount;
  unsigned fNamespaceScopeLevel;
  unsigned short *fCurrentSchemaURL;
  const unsigned short *fTargetNSURIString;
  const int *fSchemaRootElement;
  int fIncludeInfoList;
  int fImportedInfoList;
  int fImportingInfoList;
  SchemaInfo *fFailedRedefineList;
  SchemaInfo *fImportedNSList;
  SchemaInfo *fRecursingAnonTypes;
  SchemaInfo *fRecursingTypeNames;
  SchemaInfo *fNonXSAttList;
  int *fMemoryManager;
};
SchemaInfo::SchemaInfo(unsigned short, int, int, int, int, unsigned,
                       unsigned short *, const unsigned short *const,
                       const int *const, int *const)
    : fNamespaceScopeLevel(), fCurrentSchemaURL(), fTargetNSURIString(),
      fSchemaRootElement(), fMemoryManager() {}
} // namespace xercesc_2_5
