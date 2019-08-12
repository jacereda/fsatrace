#undef TRACE
//#define dbg pr
#define dbg(...)

#ifdef TRACE
//#define D do { pr("%s", __FUNCTION__ ); } while (0)
#define D do { emitOp(true, 'X', __FUNCTION__, 0); } while (0)
#else
#define D
#endif

void pr(const char *fmt, ...);

void fatal(const char *fmt,...);

void _lassert(const char *, const char *, unsigned);

#undef ASSERT

#define ASSERT(x) do { if (!(x)) _lassert(#x, __FILE__, __LINE__); } while (0)

#define CHK(x) do { if (!(x)) fatal(__FILE__ ":%d: " #x ", err: %x", __LINE__, GetLastError()); } while (0)
