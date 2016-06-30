
/* Preprocessing Template Haskell because I'm so lazy */
#define INFO(MSG) \
  $(logTM) InfoS MSG

#define DEBUG(MSG) \
  $(logTM) DebugS MSG

#define WARN(MSG) \
  $(logTM) WarnS MSG
