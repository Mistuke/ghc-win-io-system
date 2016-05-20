#include <stdlib.h>
#include <string.h>
#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>
#include <mswsock.h>
#include <stdio.h>

// WSAID macros and corresponding function signatures copied from Wine's mswsock.h

#define WSAID_ACCEPTEX \
    {0xb5367df1,0xcbac,0x11cf,{0x95,0xca,0x00,0x80,0x5f,0x48,0xa1,0x92}}
#define WSAID_CONNECTEX \
    {0x25a207b9,0xddf3,0x4660,{0x8e,0xe9,0x76,0xe5,0x8c,0x74,0x06,0x3e}}
#define WSAID_DISCONNECTEX \
    {0x7fda2e11,0x8630,0x436f,{0xa0,0x31,0xf5,0x36,0xa6,0xee,0xc1,0x57}}
#define WSAID_GETACCEPTEXSOCKADDRS \
    {0xb5367df2,0xcbac,0x11cf,{0x95,0xca,0x00,0x80,0x5f,0x48,0xa1,0x92}}
#define WSAID_TRANSMITFILE \
    {0xb5367df0,0xcbac,0x11cf,{0x95,0xca,0x00,0x80,0x5f,0x48,0xa1,0x92}}
#define WSAID_TRANSMITPACKETS \
    {0xd9689da0,0x1f90,0x11d3,{0x99,0x71,0x00,0xc0,0x4f,0x68,0xc8,0x76}}
#define WSAID_WSARECVMSG \
    {0xf689d7c8,0x6f1f,0x436b,{0x8a,0x53,0xe5,0x4f,0xe3,0x51,0xc3,0x22}}
#define WSAID_WSASENDMSG \
    {0xa441e712,0x754f,0x43ca,{0x84,0xa7,0x0d,0xee,0x44,0xcf,0x60,0x6d}}

typedef TRANSMIT_PACKETS_ELEMENT *LPTRANSMIT_PACKETS_ELEMENT;

typedef struct Winsock_s {
    // Callbacks for mswsock functions, which we have to
    // load dynamically using WSAIoctl.
    BOOL (WINAPI * AcceptEx)(SOCKET, SOCKET, PVOID, DWORD, DWORD, DWORD, LPDWORD, LPOVERLAPPED);
    BOOL (WINAPI * ConnectEx)(SOCKET, const struct sockaddr *, int, PVOID, DWORD, LPDWORD, LPOVERLAPPED);
    BOOL (WINAPI * DisconnectEx)(SOCKET, LPOVERLAPPED, DWORD, DWORD);
    VOID (WINAPI * GetAcceptExSockaddrs)(PVOID, DWORD, DWORD, DWORD, struct sockaddr **, LPINT, struct sockaddr **, LPINT);
    BOOL (WINAPI * TransmitFile)(SOCKET, HANDLE, DWORD, DWORD, LPOVERLAPPED, LPTRANSMIT_FILE_BUFFERS, DWORD);
    BOOL (WINAPI * TransmitPackets)(SOCKET, LPTRANSMIT_PACKETS_ELEMENT, DWORD, DWORD, LPOVERLAPPED, DWORD);
    INT  (WINAPI * WSARecvMsg)(SOCKET, LPWSAMSG, LPDWORD, LPWSAOVERLAPPED, LPWSAOVERLAPPED_COMPLETION_ROUTINE);
    INT  (WINAPI * WSASendMsg)(SOCKET, LPWSAMSG, DWORD, LPDWORD, LPWSAOVERLAPPED, LPWSAOVERLAPPED_COMPLETION_ROUTINE);

    WSADATA wsaData;
} Winsock;

Winsock *c_winsock_init(void)
{
    /* Allocate Winsock object */
    Winsock *winsock = HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(*winsock));
    if (winsock == NULL) {
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    }

    /* Start up Winsock */
    {
        WORD version = MAKEWORD(2, 2);
        int rc = WSAStartup(version, &winsock->wsaData);
        if (rc != 0)
            goto fail1;
        if (!(LOBYTE(winsock->wsaData.wVersion) == 2 &&
              HIBYTE(winsock->wsaData.wVersion) == 2))
            goto fail2;
    }

    /* Load mswsock functions */
    {
        SOCKET sock = socket(AF_INET, SOCK_STREAM, 0);
        if (sock == INVALID_SOCKET)
            goto fail2;

        #define get(fun, required, guid_init) \
        { \
            GUID guid = guid_init; \
            DWORD dwBytes = 0; \
            int rc = WSAIoctl(sock, SIO_GET_EXTENSION_FUNCTION_POINTER, \
                &guid, sizeof(guid), \
                &winsock->fun,  sizeof(winsock->fun), \
                &dwBytes, NULL, NULL); \
            if (rc != 0) { \
                if (required) \
                    goto fail2; \
                else \
                    winsock->fun = NULL; \
            } \
        }

        get(AcceptEx,             TRUE,  WSAID_ACCEPTEX)                // Windows 2000 Professional
        get(ConnectEx,            TRUE,  WSAID_CONNECTEX)               // Windows XP
        get(DisconnectEx,         TRUE,  WSAID_DISCONNECTEX)            // Windows XP
        get(GetAcceptExSockaddrs, TRUE,  WSAID_GETACCEPTEXSOCKADDRS)    // Windows 2000 Professional
        get(TransmitFile,         TRUE,  WSAID_TRANSMITFILE)            // Windows 2000 Professional
        get(TransmitPackets,      TRUE,  WSAID_TRANSMITPACKETS)         // Windows XP
        get(WSARecvMsg,           TRUE,  WSAID_WSARECVMSG)              // Windows XP
        get(WSASendMsg,           FALSE, WSAID_WSASENDMSG)              // Windows Vista

        #undef get
    }

    return winsock;

fail2:
    WSACleanup();
fail1:
    HeapFree(GetProcessHeap(), 0, winsock);
    return NULL;
}

#define OUTBUF_LEN ((sizeof(SOCKADDR_IN) + 16)*3)
static char outBuf[OUTBUF_LEN];

BOOL c_winsock_accept(Winsock *winsock, SOCKET listenSock,
                      SOCKET acceptSock, OVERLAPPED *ol)
{
    DWORD dwBytes = 0;
    BOOL ok = winsock->AcceptEx(listenSock, acceptSock, &outBuf, 0,
                                sizeof(SOCKADDR_IN) + 16,
                                sizeof(SOCKADDR_IN) + 16, &dwBytes, ol);
    return (ok || WSAGetLastError() == ERROR_IO_PENDING);
}

void c_winsock_accept_on_completion(SOCKET listenSock, SOCKET acceptSock)
{

    setsockopt(acceptSock, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT,
               (char *)&listenSock, sizeof(listenSock));
}


BOOL c_winsock_connect(Winsock *winsock, SOCKET sock, SOCKADDR *addr, int addrLen, OVERLAPPED *ol)
{
    // ConnectEx requires the socket to be initially bound.
    {
        struct sockaddr_in addr;
        int rc;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family      = AF_INET;
        addr.sin_addr.s_addr = INADDR_ANY;
        addr.sin_port        = 0;
        rc = bind(sock, (SOCKADDR *) &addr, sizeof(addr));
        if (rc != 0)
            return FALSE;
    }

    BOOL ok = winsock->ConnectEx(sock, addr, addrLen, NULL, 0, NULL, ol);
    return (ok || WSAGetLastError() == ERROR_IO_PENDING);
}

BOOL c_winsock_recv(SOCKET sock, char *buf, u_long bufsize, OVERLAPPED *ol)
{
    WSABUF wsabuf = {.len = bufsize, .buf = buf};
    DWORD flags = 0;
    int rc = WSARecv(sock, &wsabuf, 1, NULL, &flags, ol, NULL);
    return (rc == 0 || WSAGetLastError() == ERROR_IO_PENDING);
}

BOOL c_winsock_send(SOCKET sock, char *buf, u_long bufsize, OVERLAPPED *ol)
{
    WSABUF wsabuf = {.len = bufsize, .buf = buf};
    int rc = WSASend(sock, &wsabuf, 1, NULL, 0, ol, NULL);
    return (rc == 0 || WSAGetLastError() == ERROR_IO_PENDING);
}
