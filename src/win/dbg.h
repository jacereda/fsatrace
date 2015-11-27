#undef TRACE
//#define dbg pr
#define dbg(...)

#ifdef TRACE
#define D semit(__FUNCTION__ "\n")
#else
#define D
#endif

void pr(const char *fmt, ...);

void fatal(const char *fmt,...);

void _assert(const char *, const char *, unsigned);

#undef ASSERT

#define ASSERT(x) do { if (!(x)) _assert(#x, __FILE__, __LINE__); } while (0)

#define CHK(x) do { if (!(x)) fatal(__FILE__ ":%d: " #x ", err: %x", __LINE__, GetLastError()); } while (0)
