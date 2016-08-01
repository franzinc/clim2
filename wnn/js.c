/*
 */
/*
 * Copyright Kyoto University Research Institute for Mathematical Sciences
 *                 1987, 1988, 1989, 1990, 1991, 1992
 * Copyright OMRON Corporation. 1987, 1988, 1989, 1990, 1991, 1992
 * Copyright ASTEC, Inc. 1987, 1988, 1989, 1990, 1991, 1992
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that all of the following conditions are satisfied:
 *
 * 1) The above copyright notices appear in all copies
 * 2) Both those copyright notices and this permission notice appear
 *    in supporting documentation
 * 3) The name of "Wnn" isn't changed unless substantial modifications
 *    are made, or
 * 3') Following words followed by the above copyright notices appear
 *    in all supporting documentation of software based on "Wnn":
 *
 *   "This software is based on the original version of Wnn developed by
 *    Kyoto University Research Institute for Mathematical Sciences (KURIMS),
 *    OMRON Corporation and ASTEC Inc."
 *
 * 4) The names KURIMS, OMRON and ASTEC not be used in advertising or
 *    publicity pertaining to distribution of the software without
 *    specific, written prior permission
 *
 * KURIMS, OMRON and ASTEC make no representations about the suitability
 * of this software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * Wnn consortium is one of distributors of the official Wnn source code
 * release.  Wnn consortium also makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * KURIMS, OMRON, ASTEC AND WNN CONSORTIUM DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL KURIMS, OMRON, ASTEC OR
 * WNN CONSORTIUM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 */
/*	Version 4
 */
/*
	Nihongo Henkan Library
*/
/*
	entry functions

	js_open_lang	js_close
	js_change_current_jserver
	js_connect_lang	js_disconnect
	js_isconnect

	js_param_get	js_param_set

	js_access	js_mkdir

	js_get_lang	js_set_lang


extern	Variables
	int	wnn_errorno;
*/


/* removed for building on FC4, x86_64 */
/* extern	char	*malloc(); */

#if defined(__AARCH64EL__)
#include <bits/types.h>
#undef __FD_SETSIZE
#define __FD_SETSIZE 65536
#include <stdlib.h>
#endif

#include <stdio.h>
#include <ctype.h>
#ifdef UX386
#include <X11/Xos.h>
#else
#include <fcntl.h>
#endif
#include <pwd.h>
#ifndef UX386
#include <sys/types.h>
#endif
#include <sys/stat.h>
#include <errno.h>
extern int errno;
#include <signal.h>
#include "jd_sock.h"
#include "commonhd.h"
#include "demcom.h"
#include "config.h"

#include "wnnerror.h"
/*#include "commonhd.h"*/
#include "jslib.h"
#include "jh.h"

#include "msg.h"

#define JS			/* For include ../etc/bdic.c */
#include "bdic.c"
#include "pwd.c"

/* removed for building on FC4, x86_64 */
/* char *malloc(); */

#ifdef SYSVR2
#define	bzero(adr,n)	memset((adr),0,(n))
#endif

int	wnn_errorno	=0;
struct msg_cat *wnn_msg_cat = NULL;

/*	j Lib.	*/

static	int		current_sd;		/** ソケットfd	**/
static	WNN_JSERVER_ID	*current_js = NULL;


/*	Packet Buffers		*/
static	unsigned char	snd_buf[S_BUF_SIZ];	/** 送信 **/
static	int	sbp=0;			/** 送信バッファーポインター **/
static	int	rbc= -1;		/** 受信バッファーポインター **/

#if defined(EAGAIN)
# if defined(EWOULDBLOCK)
# define ERRNO_CHECK(no) 	((no) == EAGAIN || (no) == EWOULDBLOCK)
# else /* defined(EWOULDBLOCK) */
# define ERRNO_CHECK(no)	((no) == EAGAIN)
# endif /* defined(EWOULDBLOCK) */
#else /* defined(EAGAIN) */
# if defined(EWOULDBLOCK)
# define ERRNO_CHECK(no)	((no) == EWOULDBLOCK)
# else /* defined(EWOULDBLOCK) */
# define ERRNO_CHECK(no)	(0)
# endif /* defined(EWOULDBLOCK) */
#endif /* defined(EAGAIN) */

static void connect_timeout();
static int _get_server_name();
static int writen();
static char *get_unixdomain_of_serv_defs(), *get_service_of_serv_defs();
static int get_port_num_of_serv_defs();


/*********	V4	*****************/
/***
	jserver_dead Macro
***/

static	jmp_buf	current_jserver_dead;

#define	handler_of_jserver_dead(err_val) \
{ \
    if (current_js) { \
	if(current_js->js_dead){wnn_errorno=WNN_JSERVER_DEAD;return err_val;}\
	if(setjmp(current_jserver_dead)){ \
		wnn_errorno=WNN_JSERVER_DEAD; \
		return err_val; \
	} \
	wnn_errorno = 0; /* here initialize wnn_errorno; */    \
    } \
}

static void
set_current_js(server)
register WNN_JSERVER_ID *server;
{
 current_js = server;
 current_sd = current_js->sd;
}

/**	デーモンが死んだ時のための後始末	**/
static void
demon_dead()
{
 current_js->js_dead= -1;
 wnn_errorno= WNN_JSERVER_DEAD;
 shutdown(current_sd, 2);
 close(current_sd);
#if DEBUG
	fprintf(stderr,"jslib:JSERVER %s is Dead\n",current_js->js_name);
#endif
 if(current_js->js_dead_env_flg){
	longjmp(current_js->js_dead_env,666);
 }
 longjmp(current_jserver_dead,666);
/* never reach */
}


/**
	ソケットをオープンしてcurrent_sdにソケットfdを返す
			(cdというのはコミュニケーションデバイスの名残)
**/
static int
cd_open(lang)
register char *lang;
{
#ifdef AF_UNIX
    int sd;
    struct sockaddr_in saddr;		/** ソケット **/
    char *sock_name = NULL;
    saddr.sin_family = AF_UNIX;

    /* find socket name from table by lang */
    if (lang && *lang) {
	if ((sock_name = get_unixdomain_of_serv_defs(lang)) == NULL) {
	    sock_name = sockname;
	}
    } else {
	sock_name = sockname;		/* Jserver */
    }
    /*  strcpy(saddr.sun_path, sock_name);  FIXME */
	    
    if ((sd = socket(AF_UNIX,SOCK_STREAM, 0)) == ERROR) {
#if DEBUG
	xerror("jslib:Can't create socket.\n");
#endif
	return -1;
    }
    if (connect(sd,(struct sockaddr *)&saddr,sizeof(saddr)) == ERROR) {

#if DEBUG
	xerror("jslib:Can't connect socket.\n");
#endif
	close(sd);
	return -1;
    }
    return sd;
#else
    return -1;
#endif
}

static int
cd_open_in(server, lang, timeout)
register char *server;
register char *lang;
register int timeout;
{
    int sd;
    struct sockaddr_in saddr_in;		/** ソケット **/
    struct servent *sp = NULL;
    register struct hostent *hp;
    int serverNO, port_num;
    int ret;
    char pserver[64];
    char sserver[64];
    char *serv_name = NULL;

    serverNO = _get_server_name(server, pserver);

    /* find service name from table by lang */
    if (lang && *lang) {
	if ((serv_name = get_service_of_serv_defs(lang)) == NULL) {
	    strcpy(sserver, SERVERNAME);
	} else {
	    strcpy(sserver, serv_name);
	}
    } else {
/*
	serv_name = SERVERNAME;
*/
	strcpy(sserver, SERVERNAME);
    }
/*
    if ((sp = getservbyname(serv_name,"tcp")) != NULL) {
*/
    if ((sp = getservbyname(sserver,"tcp")) != NULL) {
	serverNO += ntohs(sp->s_port);
    } else {
	if ((port_num = get_port_num_of_serv_defs(lang)) == -1) {
	    serverNO += WNN_PORT_IN;
	} else {
	    serverNO += port_num;
	}
    }
    if ((hp = gethostbyname(pserver)) == NULL) { return  -1; }
    bzero((char *)&saddr_in,sizeof(saddr_in));
    bcopy(hp->h_addr,(char *)&saddr_in.sin_addr, hp->h_length);
    saddr_in.sin_family = AF_INET;
    saddr_in.sin_port = htons(serverNO);
    if ((sd = socket(AF_INET, SOCK_STREAM, 0)) == ERROR){
#if DEBUG
	xerror("jslib:Can't create Inet socket.\n");
#endif
	return -1 ;
    }

    if (timeout != 0 && timeout > 0) {
	signal(SIGALRM, connect_timeout);
	alarm(timeout);
    }
    ret = connect(sd, (struct sockaddr *)&saddr_in, sizeof(saddr_in));
    if (timeout != 0 && timeout > 0) {
	alarm(0);
	signal(SIGALRM, SIG_IGN);
    }
    if (ret == ERROR) {
#if DEBUG
	xerror("jslib:Can't connect Inet socket.\n");
#endif
	close(sd);
	return -1 ;
    }
    return sd;
}

static void
connect_timeout()
{
}

/* get server name and return serverNo */
static int
_get_server_name(server, pserver)
char *server;
char *pserver;
{
    register char *p;
    strcpy(pserver, server);
    p = pserver;
    for( ; *p && *p != ':'; p++) ;
    if (!*p) return(0);	/* does not have a colon */
    *p = 0;
    return(atoi(++p));
}

/*	Packet SND/RCV subroutines	*/
static void put4com();

/**	パケットのヘッダーを送る	**/
static void
snd_head(cmd)
int cmd;	/** cmd=コマンド **/
{
 sbp=0;
 put4com(cmd);
 rbc= -1;
}

/**	パケットのヘッダーを送る	**/
static int
snd_env_head(env,cmd)
register struct wnn_env *env;
int cmd;	/** cmd=コマンド **/
{
 snd_head(cmd);
 put4com(env->env_id);
 return 0;
}

/**	パケットのヘッダーを送る	**/
static int
snd_server_head(server,cmd)
register WNN_JSERVER_ID *server;
int cmd;	/** cmd=コマンド **/
{
 snd_head(cmd);
 return 0;
}

/**	送信バッファをフラッシュする	**/
static int
snd_flush()
{
 if(sbp==0)return(-1);
 writen(sbp);
 sbp=0;
 return(0);
}

static int
writen(n)
int n;
{int cc,x;
 for(cc=0;cc<n;){
	errno = 0;
	x=write(current_sd, &snd_buf[cc],n-cc );
	if(x < 0) {
	    if (ERRNO_CHECK(errno) || errno == EINTR) {
		continue;
	    } else {
		demon_dead();
		return -1;
	    }
	}
	cc+=x;
 }
#if DEBUG
	fprintf(stderr,"jslib:writen=%d\n",x);
/*	dmp(snd_buf,x); */
#endif
 return(0);
}

/**	サーバへ1バイト送る	**/
static void
put1com(c)
int c;
{
 snd_buf[sbp++]=c;
 if(sbp>=S_BUF_SIZ){ 
	writen(S_BUF_SIZ);
	sbp=0;
 }
}

/**	サーバへ2バイト送る	**/
static void
put2com(c)
int c;
{
 put1com(c>>(8*1));
 put1com(c       );
}

/**	サーバへ4バイト送る	**/
static void
put4com(c)
int c;
{
 put1com(c>>(8*3));
 put1com(c>>(8*2));
 put1com(c>>(8*1));
 put1com(c       );
}

/**	サーバへ文字列を送る	**/
static void
putwscom(p) register w_char *p;
{
 if(p==NULL){ put2com(0); return; }
 while(*p)put2com(*p++);
 put2com(0);
}

/**	サーバへ文字列を送る	**/
static void
putscom(p) register char *p;
{
 if(p==NULL){ put1com(0); return; }
 while(*p)put1com(*p++);
 put1com(0);
}

/**	サーバから1バイト受ける	**/
static int
get1com()
{static int rbp;
 static	unsigned char	rcv_buf[R_BUF_SIZ];	/** 受信 **/
 if(rbc<=0){
    while(1) {
	errno = 0;
	rbc = read(current_sd, rcv_buf, R_BUF_SIZ);
	if(rbc <= 0) {
	    if (ERRNO_CHECK(errno)) {
		continue;
	    } else if (rbc == 0) {
		demon_dead();
		return -1;
	    } else {	/* cc == -1 */
		if (errno != EINTR) {
		    demon_dead();
		    return -1;
		}
		continue;
	    }
	}
	rbp=0;
#if DEBUG
	fprintf(stderr,"jslib:read:rbc=%d\n",rbc);
/*	dmp(rcv_buf,rbc); */
#endif
	break;
    }
 }
 rbc--;
 return rcv_buf[rbp++] & 0xFF ;
}

/**	サーバから2バイト受ける	**/
static int
get2com()
{register int h;
 h=get1com();
 return (h<<8) | get1com();
}

/**	サーバから4バイト受ける	**/
static int
get4com()
{register int h1,h2,h3;
 h1=get1com() << 24 ;
 h2=get1com() << 16 ;
 h3=get1com() <<  8 ;
 return h1 | h2 | h3 | get1com();
}

/**	サーバへ文字列を送る	**/
static void
getscom(p) register char *p;
{
 while(*p++= get1com())
	;
}

/**	サーバへ文字列を送る	**/
static void
getwscom(p) w_char register *p;
{
 while(*p++= get2com())
	;
}

#ifdef nodef
/* Moved from ../etc/string.c */
/**	**/
static
int Strlen(s)
register w_char *s;
{
    register int n;

    for (n = 0;*s++ != 0;n++);
    return n;
}

/**	**/
static 
w_char *Strcpy(s1,s2)
register w_char *s1;
register w_char *s2;
{
	register w_char *d;

	for (d = s1;(*d++ = *s2++) != 0;);
	return s1;
}
#endif


/*	Debug Subroutines	*/
#if DEBUG
void
xerror(s)char *s;
{
 fprintf(stderr,"%s\n",s);
}

void
dmp(p,c)char *p;
{
 int i,j;
 for(i=0;;i+=16){
	for(j=0;j<16;j++){
	    	if(c<=0){	fprintf(stderr,"\n"); return;}
		fprintf(stderr,"%02x ",p[i+j]&0xFF);
		c--;
	}
	fprintf(stderr,"\n");
 }
}

#endif

/*	get login name form /etc/passwd file	*/
static char*	
getlogname()
{
 struct passwd *getpwuid();
 return getpwuid(getuid())->pw_name;
}




/*
 *		Lib. Functions
 *		raw lib.
 */

/***
	js
	・global
***/

/**	  jserver と接続する。jserver_id を返す。	**/
WNN_JSERVER_ID *
js_open_lang(server, lang, timeout)
register char *server, *lang;
register int timeout;
{char *new_js;
 char host[WNN_HOSTLEN],user[WNN_ENVNAME_LEN];
 int x;

 if (wnn_msg_cat == NULL){
    char nlspath[64];
    strcpy(nlspath, LIBDIR);
    strcat(nlspath, "/%L/%N");
    wnn_msg_cat = msg_open("libwnn.msg", nlspath, lang);
    if(wnn_msg_cat == NULL){
	fprintf(stderr, "libwnn: Cannot open message file for libwnn.a\n");
    }
 }
 sbp=0;	/* init sndBufPointer */
 if(!(new_js=(char *)malloc(sizeof(WNN_JSERVER_ID)))){
 				wnn_errorno=WNN_ALLOC_FAIL;return NULL;
 }
 current_js =(WNN_JSERVER_ID *) new_js;
 if (server == NULL) {
   current_js->js_name[0] = '\0';
 } else {
   strncpy(current_js->js_name, server, sizeof(current_js->js_name) - 1);
   current_js->js_name[sizeof(current_js->js_name) - 1] = '\0';
 }
 current_js->js_dead= 0;
 current_js->js_dead_env_flg= 0;
/*
 if(user == NULL || 0==strcmp(user,""))
*/
 strncpy(user, getlogname(), WNN_ENVNAME_LEN);
 user[WNN_ENVNAME_LEN-1] = '\0';	/* truncate by WNN_ENVNAME_LEN */
 if(server == NULL || 0==strcmp(server,"") || 0==strcmp(server,"unix")){
   strcpy(host,"unix");
   if((current_sd= cd_open(lang))==-1){
	wnn_errorno=WNN_SOCK_OPEN_FAIL;free((char*)current_js);current_js=NULL;
	return NULL;
   }
 }else{
   gethostname(host,WNN_HOSTLEN);
   host[WNN_HOSTLEN-1] = '\0';	/* truncate by WNN_HOSTLEN */
   if((current_sd= cd_open_in(server, lang, timeout))==-1){
	wnn_errorno=WNN_SOCK_OPEN_FAIL;free((char*)current_js);current_js=NULL;
	return NULL;
   }
 }
 current_js->sd= current_sd;
 handler_of_jserver_dead(((WNN_JSERVER_ID *)NULL));
 snd_head(JS_OPEN);
 put4com(JLIB_VERSION);		/* H.T. */
 putscom(host);
 putscom(user);
 snd_flush();
 if(get4com()==-1){
     x = wnn_errorno=get4com();
     js_close(current_js);		/* H.T. */
     current_js = NULL;
     wnn_errorno = x;
     return NULL;
 }
 return current_js;
}


/**	ソケットをクローズする	**/
/**	  jserver との接続を close する。	**/
int
js_close(server)
WNN_JSERVER_ID *server;
{
 register int x;
 WNN_JSERVER_ID tmp_js_id;
 if(server==0) return(-1);
 tmp_js_id = *server;
 free((char *)server);
 current_js = &tmp_js_id;
 current_sd = current_js->sd;
/*	handler of jserver dead */
 handler_of_jserver_dead(-1);
 snd_head(JS_CLOSE);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno=get4com();
 close(current_sd);
 return x;
}


/*
	jserver との間に connection を張り、同時に jserver の内部に環
	境を作る。env_name に既に存在する環境を指定した時にはその環境を
	返し、NULL を指定した時には新しい環境を作って返す。
*/

struct wnn_env *
js_connect_lang(server, env_name, lang)
register char *env_name;
WNN_JSERVER_ID *server;
char *lang;
{
    register int e_id;
    register struct wnn_env *env;
    void js_set_lang();

    set_current_js(server);
    if(!(env=(struct wnn_env *)malloc(sizeof(struct wnn_env)))){
	    wnn_errorno=WNN_ALLOC_FAIL;return NULL;
    }
    handler_of_jserver_dead(((struct wnn_env *)NULL));
    snd_head(JS_CONNECT);
    putscom(env_name);
    snd_flush();
    e_id=get4com();
    if(e_id==-1){ wnn_errorno= get4com(); free(env); return NULL; }
    env->env_id = e_id;
    env->js_id  = server;
    strcpy(env->lang, lang);        /* set language name */
    return env;
}

#ifdef	nodef
/* set language value to env */
void
js_set_lang(env, lang)
struct wnn_env *env;
register char *lang;
{
    register char *p;
    extern char *getenv();

    /* if not specified language , use $LANG */
    if (lang == 0 || *lang == 0)
	lang = getenv("LANG");
    if (lang == 0 || *lang == 0)
	lang = WNN_DEFAULT_LANG;
    for(p = env->lang; *lang != '.' && *lang != '@' && *lang != 0; lang++, p++)
	*p = *lang;
    *p = 0;
}
#endif

/* get language value from env */
char *
js_get_lang(env)
struct wnn_env *env;
{
	return(env->lang);
}

int
js_env_exist(server,env_name)
register char *env_name;
register WNN_JSERVER_ID *server;
{
 set_current_js(server);
 handler_of_jserver_dead(-1);
 snd_head(JS_ENV_EXIST);
 putscom(env_name);
 snd_flush();
 return(get4com());
}

int
js_env_sticky(env)
register struct wnn_env *env;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_ENV_STICKY);
 snd_flush();
 return(get4com());
}

int
js_env_un_sticky(env)
register struct wnn_env *env;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_ENV_UN_STICKY);
 snd_flush();
 return(get4com());
}

/**
	  env で示される環境を無くす。
**/
int
js_disconnect(env)
register struct wnn_env *env;
{register int x;
 struct wnn_env tmp_env;
 if(env==0) return(-1);
 tmp_env= *env;
 /* 本来は、free しなきゃあかんのだけど、リソース管理が出来ないし、
    まあ、8バイトだから、ゴミが残るけどいいだろう。
 free((char *)env);
 */
 set_current_js(env->js_id);
 handler_of_jserver_dead((int)NULL);
 snd_env_head(&tmp_env,JS_DISCONNECT);
 snd_flush();
 x=get4com();
 if(x==-1){ wnn_errorno= get4com(); }
 return x;
}

/**	サーバとコネクトしているか	**/
int
js_isconnect(env)
register struct wnn_env *env;
{
    if (env && env->js_id)
	return(env->js_id->js_dead);
    return(-1);
}

/**
	  env の 環境 との通信バッファを flush する。
**/
void
js_flush(env)
struct wnn_env *env;
{
}



/*	Parameter set/get	*/
/**	変換 parameter を設定する。	**/
/**	js_param_set		**/
int
js_param_set(env,para)
struct wnn_env *env;
register struct wnn_param *para;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_PARAM_SET);
 put4com(para->n);	/* Ｎ(大)文節解析のＮ */
 put4com(para->nsho);	/* 大文節中の小文節の最大数 */
 put4com(para->p1);	/* 幹語の頻度のパラメータ */
 put4com(para->p2);	/* 小文節長のパラメータ */
 put4com(para->p3);	/* 幹語長のパラメータ */
 put4com(para->p4);	/* 今使ったよビットのパラメータ */
 put4com(para->p5);	/* 辞書のパラメータ */
 put4com(para->p6);	/* 小文節の評価値のパラメータ */
 put4com(para->p7);	/* 大文節長のパラメータ */
 put4com(para->p8);	/* 小文節数のパラメータ */

 put4com(para->p9);	/* 疑似品詞 数字の頻度 */
 put4com(para->p10);	/* 疑似品詞 カナの頻度 *//* CWNN 英数の頻度 */
 put4com(para->p11);	/* 疑似品詞 英数の頻度 *//* CWNN 記号の頻度 */
 put4com(para->p12);	/* 疑似品詞 記号の頻度 *//* CWNN 開括弧の頻度 */
 put4com(para->p13);	/* 疑似品詞 閉括弧の頻度 *//* CWNN 閉括弧の頻度 */
 put4com(para->p14);	/* 疑似品詞 付属語の頻度 *//* BWNN No of koho */
 put4com(para->p15);	/* 疑似品詞 開括弧の頻度 *//* CWNN Not used */

 snd_flush();
 x=get4com();
 if(x==-1){ wnn_errorno= get4com(); return -1; }
 return 0;
}

/**	js_param_get		**/
/**	env で示される環境の変換 parameter を取り出す。	**/
int
js_param_get(env,para)
struct wnn_env *env;
register struct wnn_param *para;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_PARAM_GET);
 snd_flush();
 if(get4com() == -1){
     wnn_errorno= get4com(); return -1;
 }
 para->n=get4com();	/* Ｎ(大)文節解析のＮ */
 para->nsho=get4com();	/* 大文節中の小文節の最大数 */
 para->p1=get4com();	/* 幹語の頻度のパラメータ */
 para->p2=get4com();	/* 小文節長のパラメータ */
 para->p3=get4com();	/* 幹語長のパラメータ */
 para->p4=get4com();	/* 今使ったよビットのパラメータ */
 para->p5=get4com();	/* 辞書のパラメータ */
 para->p6=get4com();	/* 小文節の評価値のパラメータ */
 para->p7=get4com();	/* 大文節長のパラメータ */
 para->p8=get4com();	/* 小文節数のパラメータ */
 para->p9=get4com();	/* 疑似品詞 数字の頻度 */
 para->p10=get4com();	/* 疑似品詞 カナの頻度 */
 para->p11=get4com();	/* 疑似品詞 英数の頻度 */
 para->p12=get4com();	/* 疑似品詞 記号の頻度 */
 para->p13=get4com();	/* 疑似品詞 閉括弧の頻度 */
 para->p14=get4com();	/* 疑似品詞 付属語の頻度 */
 para->p15=get4com();	/* 疑似品詞 開括弧の頻度 */

 return 0;
}

/*
	global File Operation
*/
/**	js_mkdir	**/
int
js_mkdir(env,path)
struct wnn_env *env;
char *path;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_MKDIR);
 putscom(path);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_access	**/
int
js_access(env,path,amode)
struct wnn_env *env;
char *path;
int amode;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_ACCESS);
 put4com(amode);
 putscom(path);
 snd_flush();
 x=get4com();
 return x;
}

/**	js_file_list_all	**/
static int rcv_file_list();

int
js_file_list_all(server,ret)
WNN_JSERVER_ID *server;
struct wnn_ret_buf *ret;
{
 set_current_js(server);
 handler_of_jserver_dead(-1);
 snd_server_head(server,JS_FILE_LIST_ALL);
 snd_flush();
 return rcv_file_list(ret);
}


/**	js_file_list	**/
int
js_file_list(env,ret)
struct wnn_env *env;
struct wnn_ret_buf *ret;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FILE_LIST);
 snd_flush();
 return rcv_file_list(ret);
}

static void re_alloc();

static int
rcv_file_list(ret)
struct wnn_ret_buf *ret;
{register int i,count;
 WNN_FILE_INFO_STRUCT	*files;
 count=get4com();
 re_alloc(ret,sizeof(WNN_FILE_INFO_STRUCT)*count);
 files=(WNN_FILE_INFO_STRUCT *)ret->buf;
 for(i=0;i<count;i++){
	files->fid= get4com();
	files->localf= get4com();
	files->ref_count= get4com();
	files->type= get4com();
	getscom(files->name);
	files++;
 }
 return count;
}

/**	js_file_stat	**/
int
js_file_stat(env,path, s)
struct wnn_env *env;
char *path;
WNN_FILE_STAT *s;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FILE_STAT);
 putscom(path);
 snd_flush();
 x=get4com();
 s->type = x;
 return x;
}


/**	js_file_info	**/
int
js_file_info(env,fid,file)
struct wnn_env *env;
int fid;
register WNN_FILE_INFO_STRUCT *file;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FILE_INFO);
 put4com(fid);
 snd_flush();
 file->fid= fid;
 x=get4com();
 if(x==-1){
     wnn_errorno= get4com();
     return(-1);
 }
 getscom(file->name);
 file->localf= get4com();
 file->ref_count= get4com();
 file->type= get4com();
 return 0;
}

/**	js_file_loaded	**/
int
js_file_loaded(server,path)
WNN_JSERVER_ID *server;
char *path;
{register int x;
 set_current_js(server);
 handler_of_jserver_dead(-1);
 snd_server_head(server,JS_FILE_LOADED);
 putscom(path);
 snd_flush();
 x=get4com();
 return x;
}

/**	js_file_loaded_local	**/
static int check_local_file();
static int file_loaded_local();

int
js_file_loaded_local(server,path)
WNN_JSERVER_ID *server;
char *path;
{int x;
 set_current_js(server);
 handler_of_jserver_dead(-1);

 if(check_local_file(path) == -1) return (-1);
 snd_server_head(server, JS_FILE_LOADED_LOCAL);
 x=file_loaded_local(path);
 if(x==-1){ return -1; }
 return x;
}

static int
check_local_file(path)
char *path;
{
 register FILE *f;
 register int x;
 struct wnn_file_head fh;

#ifdef WRITE_CHECK
 check_backup(path);
#endif /* WRITE_CHECK */
 f=fopen(path,"r");
 if(f == NULL){
     wnn_errorno = WNN_OPENF_ERR;
     return -1;
 }
 x=input_file_header(f, &fh);
 if(x==-1){
     fclose(f);
     wnn_errorno = WNN_NOT_A_FILE;
     return -1;
 }
 if(check_inode(f, &fh) == -1){
     change_file_uniq(&fh, path);
#ifdef WRITE_CHECK
     fclose(f);
     f=fopen(path,"r");
     if(f == NULL){
	 wnn_errorno = WNN_OPENF_ERR;
	 return (-1);
     }
#endif /* WRITE_CHECK */
     if(check_inode(f, &fh) == -1){
	 fclose(f);
	 wnn_errorno = WNN_INODE_CHECK_ERROR;
	 return (-1);
     }
 }
 fclose(f);
 return 0;
}


static int
file_loaded_local(path)
char *path;
{register int x,i;
 FILE *f;
 struct wnn_file_head fh;

#ifdef WRITE_CHECK
 check_backup(path);
#endif /* WRITE_CHECK */
 f=fopen(path,"r");
 if(f == NULL){
     wnn_errorno = WNN_OPENF_ERR;
     return -1;
 }
 x=input_file_header(f, &fh);
 if(x==-1){
     fclose(f);
     wnn_errorno = WNN_NOT_A_FILE;
     return -1;
 }
 put4com(fh.file_uniq.time);
 put4com(fh.file_uniq.dev);
 put4com(fh.file_uniq.inode);
 for(i=0;i<WNN_HOSTLEN;i++){
	put1com(fh.file_uniq.createhost[i]);
 }

 snd_flush();
 x=get4com();
 fclose(f);
 return x;
}


/**	js_hindo_file_create	**/
int
js_hindo_file_create(env,fid,fn,comment,hpasswd)
struct wnn_env *env;
int	fid;
char *fn;
w_char *comment;
char *hpasswd;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_HINDO_FILE_CREATE);
 put4com(fid);
 putscom(fn);
 putwscom(comment);
 putscom(hpasswd);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_dic_file_create	**/
int
js_dic_file_create(env,fn,type,comment,passwd, hpasswd)
struct wnn_env *env;
char *fn;
w_char *comment;
char *passwd, *hpasswd;
int type;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_DIC_FILE_CREATE);
 putscom(fn);
 putwscom(comment);
 putscom(passwd);
 putscom(hpasswd);
 put4com(type);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}


/**	js_file_discard	**/
int
js_file_discard(env,fid)
struct wnn_env *env;
int	fid;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FILE_DISCARD);
 put4com(fid);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_file_read	**/
int
js_file_read(env,fn)
struct wnn_env *env;
char	*fn;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FILE_READ);
 putscom(fn);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_file_write	**/
int
js_file_write(env,fid,fn)
struct wnn_env *env;
int	fid;
char	*fn;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FILE_WRITE);
 put4com(fid);
 putscom(fn);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_file_receive	**/
static int xget1com();
static void xput1com();

int
js_file_receive(env,fid,fn)
struct wnn_env *env;
int	fid;
char	*fn;
{register int mode, x;
 char file_name[1024];
 char buf[1024];
 FILE *f;
 int n;
 struct wnn_file_head fh;
 int i;
#ifdef WRITE_CHECK
 char *tmp, *backup = NULL, tmp_x;
 int tmp_err = 0;
#endif /* WRITE_CHECK */

 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FILE_RECEIVE);
 put4com(fid);
 snd_flush();
/**/
 getscom(file_name);
 if(fn ==NULL || strcmp(fn,"")==0){
     gethostname(buf, 1024);
     n = strlen(buf);
     buf[n] = C_LOCAL;
     buf[n+1] = 0;
     if(strncmp(buf, file_name, n + 1) == 0){
	 fn = file_name + n + 1;
     }
 }
#ifdef WRITE_CHECK
 check_backup(fn);
#endif /* WRITE_CHECK */
 if((f = fopen(fn, "r")) == NULL){ /* New File */
     fh.file_uniq.time = fh.file_uniq.dev = fh.file_uniq.inode = 0;
 }else{			/* Old File Exists */
     if(input_file_header(f, &fh) == -1){
	 wnn_errorno=WNN_NOT_A_FILE;
	 fclose(f);
	 put4com(-1);snd_flush();
	 return(-1);
     }
     fclose(f);
 }
 put4com(0);			/* Ack */
 put4com(fh.file_uniq.time);
 put4com(fh.file_uniq.dev);
 put4com(fh.file_uniq.inode);
 for(i=0;i<WNN_HOSTLEN;i++){
     put1com(fh.file_uniq.createhost[i]);
 }

 snd_flush();

 if((mode=get4com())==-1){ /* check stat */
	wnn_errorno= get4com();
	return -1;
 }else if(mode==0){
	return 0; /* need not saving */
 }else if(mode == 1 || mode == 3){ /* mode == 3 means the file is a new one. */
#ifdef WRITE_CHECK
     backup = make_backup_file(fn);
     if ((tmp = make_tmp_file(fn, 0, &f)) == NULL) {
	 delete_tmp_file(backup);
#else /* WRITE_CHECK */
     if((f = fopen(fn, "w+")) == NULL){ 
#endif /* WRITE_CHECK */
	 wnn_errorno=WNN_FILE_WRITE_ERROR;
	 put4com(-1);snd_flush();
	 return(-1);
     }
 }else if(mode == 2){
#ifdef WRITE_CHECK
     backup = make_backup_file(fn);
     if ((tmp = make_tmp_file(fn, 1, &f)) == NULL) {
	 delete_tmp_file(backup);
#else /* WRITE_CHECK */
     if((f = fopen(fn, "r+")) == NULL){ /* New File */
#endif /* WRITE_CHECK */
	 wnn_errorno=WNN_FILE_WRITE_ERROR;
	 put4com(-1);snd_flush();
	 return(-1);
     }
 }
 put4com(0); snd_flush(); /* ACK */
 for(;;){
	if((x=xget1com())== -1) break; /* EOF */
#ifdef WRITE_CHECK
	tmp_x = (char)x;
	if (fwrite(&tmp_x, sizeof(char), 1, f) == -1) tmp_err = 1;
#else /* WRITE_CHECK */
	fputc(x,f);
#endif /* WRITE_CHECK */
 }
 fclose(f);
#ifdef WRITE_CHECK
 if (tmp_err == 0) {
     move_tmp_to_org(tmp, fn, 1);
 } else {
     delete_tmp_file(tmp);
 }
 delete_tmp_file(backup);
#endif /* WRITE_CHECK */

 x=get4com();
 if(x==-1)wnn_errorno= get4com();
#ifdef WRITE_CHECK
 if (tmp_err) {
     wnn_errorno = WNN_FILE_WRITE_ERROR;
     return(-1);
 }
#endif /* WRITE_CHECK */

 return x;
}

static int
xget1com()
{register int x;
 if((x= get1com()) != 0xFF) return x;
 if(get1com() == 0xFF) return -1; /* EOF */
 return 0xFF;
}

/**	js_file_send	**/
int
js_file_send(env,fn)
struct wnn_env *env;
char	*fn;
{register int x;
 FILE *f;
 int n;
 char buf[1024],*b;
 register int cc,i;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);

 if(check_local_file(fn) == -1) return (-1);

 snd_env_head(env,JS_FILE_SEND);
 x=file_loaded_local(fn);
 if(x!= -1){ /* file is already loaded */
     if(get4com() == -1) {
	 wnn_errorno= get4com();
	 return(-1);
     }
     return x;
 }

 x=get4com();
 if(x==-1){
	wnn_errorno= get4com();
	return -1;
 }

 gethostname(buf, 1024);
 n = strlen(buf);
 buf[n] = C_LOCAL;
 strcpy(buf + n + 1,fn);
 putscom(buf);

#ifdef WRITE_CHECK
 check_backup(fn);
#endif /* WRITE_CHECK */
 if((f=fopen(fn,"r"))== NULL){
	xput1com(-1); /* EOF */
	return -1;
 }

 /* send contents of file */
 for(;;){
	cc = fread(buf,1,1024,f);
	if(cc <= 0) break; /* EOF */
	for(b=buf,i=0;i<cc;i++){
		xput1com((int)*b++ & 0xff);
	}
 }
 fclose(f);
 xput1com(-1); /* EOF */
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

static void
xput1com(d)
int d;
{
 if(d == -1){ put1com(0xFF); put1com(0xFF);return;/* EOF */}
 put1com(d);
 if(d == 0xFF){ put1com(0x00);}
}


/***	Dic. Operation for Env.	 ***/

/**	js_dic_add	**/
int
js_dic_add(env,fid,hfid,rev, jnice,rw,hrw, pw1, pw2)
struct wnn_env *env;
int	fid,hfid,rev,jnice,rw,hrw;
char *pw1, *pw2;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_DIC_ADD);
 put4com(fid);
 put4com(hfid);
 put4com(jnice);
 put4com(rw);
 put4com(hrw);
 putscom(pw1);
 putscom(pw2);
 put4com(rev);		/* rev is to add it as reverse dict */
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_dic_delete	**/
int
js_dic_delete(env,dicno)
struct wnn_env *env;
int	dicno;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_DIC_DELETE);
 put4com(dicno);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_dic_use	**/
int
js_dic_use(env,dic_no,flag)
struct wnn_env *env;
int	dic_no,flag;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_DIC_USE);
 put4com(dic_no);
 put4com(flag);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_fuzokugo_set	**/
int
js_fuzokugo_set(env,fid)
struct wnn_env *env;
int	fid;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FUZOKUGO_SET);
 put4com(fid);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_fuzokugo_get	**/
int
js_fuzokugo_get(env)
struct wnn_env *env;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FUZOKUGO_GET);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_dic_list_all	**/
static int rcv_dic_list();
static void get_dic_info();

int
js_dic_list_all(server,ret)
WNN_JSERVER_ID *server;
struct wnn_ret_buf *ret;
{
 set_current_js(server);
 handler_of_jserver_dead(-1);
 snd_server_head(server,JS_DIC_LIST_ALL);
 snd_flush();
 return rcv_dic_list(ret);
}


/**	js_dic_list	**/
int
js_dic_list(env,ret)
struct wnn_env *env;
struct wnn_ret_buf *ret;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_DIC_LIST);
 snd_flush();
 return rcv_dic_list(ret);
}

static int
rcv_dic_list(ret)
struct wnn_ret_buf *ret;
{register int i,count;
 register WNN_DIC_INFO *dic;
 count=get4com();
 re_alloc(ret, sizeof(WNN_DIC_INFO)*(count + 1));

 dic=(WNN_DIC_INFO *)ret->buf;
 for(i=0;i<count;i++){
     get_dic_info(dic);
     dic++;
 }
 dic->dic_no = -1;
 return count;
}

static void
get_dic_info(dic)
register WNN_DIC_INFO *dic;
{
	dic->dic_no =get4com();	/* dic_No */
	dic->body =get4com();	/* body fid */
	dic->hindo =get4com();	/* hindo fid */
	dic->rw =get4com();	/* r/w */
	dic->hindo_rw =get4com();	/* hindo r/w */
	dic->enablef =get4com();	/* enable/disable */
	dic->nice =get4com();	/* nice */
	dic->rev = get4com();
/* added H.T */
	getwscom(dic->comment);
	getscom(dic->fname);
	getscom(dic->hfname);
	getscom(dic->passwd);
	getscom(dic->hpasswd);
	dic->type = get4com();
	dic->gosuu = get4com();
	dic->localf = get4com();
	dic->hlocalf = get4com();
}

/***	Dic. Operation by dic_No.	***/

/**	js_word_add		**/
int
js_word_add(env,dic_no,yomi,kanji,comment,hinshi,init_hindo)
struct wnn_env *env;
int	dic_no;
w_char	*yomi,*kanji,*comment;
int	hinshi,init_hindo;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_WORD_ADD);
 put4com(dic_no);
 putwscom(yomi);
 putwscom(kanji);
 putwscom(comment);
 put4com(hinshi);
 put4com(init_hindo);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}

/**	js_word_delete		**/
int
js_word_delete(env,dic_no,entry)
struct wnn_env *env;
int	dic_no;
int	entry;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_WORD_DELETE);
 put4com(dic_no);
 put4com(entry);
 snd_flush();
 x=get4com();
 if(x==-1)wnn_errorno= get4com();
 return x;
}


/**	js_word_search		**/
static int rcv_word_data();

int
js_word_search(env,dic_no,yomi,ret)
struct wnn_env *env;
int	dic_no;
w_char	*yomi;
struct wnn_ret_buf *ret;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_WORD_SEARCH);
 put4com(dic_no);
 putwscom(yomi);
 snd_flush();

 return (rcv_word_data(ret, yomi));
}

/**	js_word_search_by_env	**/
int
js_word_search_by_env(env,yomi,ret)
struct wnn_env *env;
w_char	*yomi;
struct wnn_ret_buf *ret;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_WORD_SEARCH_BY_ENV);
 putwscom(yomi);
 snd_flush();

 return (rcv_word_data(ret, yomi));
}

/**	js_word_info		**/
int
js_word_info(env,dic_no,entry,ret)
struct wnn_env *env;
int	dic_no,entry;
struct wnn_ret_buf *ret;
{register int x;
 w_char yomi[LENGTHYOMI];

 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_WORD_INFO);
 put4com(dic_no);
 put4com(entry);
 snd_flush();
 x=get4com();
 if(x==-1){
     wnn_errorno= get4com();
     return(-1);
 }
 getwscom(yomi);
 rcv_word_data(ret, yomi);
 return(0);
}

int
js_word_comment_set(env, dic_no, entry, comment)
struct wnn_env *env;
int	dic_no,entry;
w_char *comment;
{
 register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_WORD_COMMENT_SET);
 put4com(dic_no);
 put4com(entry);
 putwscom(comment);
 snd_flush();
 x=get4com();
 if(x==-1){
     wnn_errorno= get4com();
     return(-1);
 }
 return(0);
}    

/**	rcv for word_search		**/
static int
rcv_word_data(ret, yomi)
struct wnn_ret_buf *ret;
w_char *yomi;			/* Yomi is not sent from server
				 *  (at least for the time being).
				 */
{register int x, j_c,k_c;
 w_char *k;
 register struct wnn_jdata *jd;
 register int cnt;

 j_c = get4com();
 k_c = get4com();
 re_alloc(ret, sizeof(struct wnn_jdata)*(j_c + 1) + sizeof(w_char)*(k_c + j_c * 3 + j_c * wnn_Strlen(yomi)));
 jd=(struct wnn_jdata *)ret->buf;
 for(cnt = 0;;cnt++){
	jd->dic_no = x = get4com();
	if(x==-1) break;
	jd->serial  = get4com();
	jd->hinshi = get4com();
	jd->hindo = get4com();	jd->ima = get4com();
	jd->int_hindo = get4com();	jd->int_ima = get4com();
	jd++;
 }
 jd++;
 k= (w_char *)jd;
 jd=(struct wnn_jdata *)ret->buf;
 for(;;){
	if(jd->dic_no==-1) break;

	jd->yomi = k;		/* Copy Yomi */
	wnn_Strcpy(k, yomi);
	k+= wnn_Strlen(k)+1;

	jd->kanji = k;		/* Get Kanji */
	getwscom(k);
	k+= wnn_Strlen(k)+1;

	jd->com = k;		/* Get Comment */
	getwscom(k);
	k+= wnn_Strlen(k)+1;
	jd++;
 }
 return cnt;
}


/**	js_dic_info	**/
int
js_dic_info(env,dic_no,ret)
struct wnn_env *env;
int	dic_no;
register WNN_DIC_INFO *ret;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_DIC_INFO);
 put4com(dic_no);
 snd_flush();
 x=get4com();
 if(x==-1){ return get4com();}
 get_dic_info(ret);
 return dic_no;
}


/**	js_who		**/
int
js_who(server,ret)
WNN_JSERVER_ID *server;
struct wnn_ret_buf *ret;
{register int i,j,c;
 WNN_JWHO *w;
 set_current_js(server);
 handler_of_jserver_dead(-1);
 snd_server_head(server,JS_WHO);
 snd_flush();

 c=get4com();
 if(c==-1){ wnn_errorno= get4com();return -1;}

 re_alloc(ret, sizeof(WNN_JWHO)*c);
 w=(WNN_JWHO *)ret->buf;
 for(i=0;i<c;i++){
	w->sd=get4com();
	getscom(w->user_name);
	getscom(w->host_name);
	for(j=0;j<WNN_MAX_ENV_OF_A_CLIENT;j++){
		(w->env)[j]=get4com();
	}
	w++;
 }
 return(c);
}

/**	jserver 中の全ての環境に関する情報を得る。
	(ウラ技)
**/
int
js_env_list(server,ret)
WNN_JSERVER_ID *server;
struct wnn_ret_buf *ret;
{register int i,j,c;
 WNN_ENV_INFO *w;
 set_current_js(server);
 handler_of_jserver_dead(-1);
 snd_server_head(server,JS_ENV_LIST);
 snd_flush();

 c=get4com();
 if(c==-1){ wnn_errorno= get4com();return -1;}

 re_alloc(ret, sizeof(WNN_ENV_INFO)*c);
 w=(WNN_ENV_INFO *)ret->buf;
 for(i=0;i<c;i++){
	w->env_id = get4com();     
	getscom(w->env_name);
	w->ref_count=get4com();
	w->fzk_fid=get4com();
	w->jishomax=get4com();
	for(j=0;j<WNN_MAX_JISHO_OF_AN_ENV;j++){
		(w->jisho)[j]= get4com();
	}
	for(j=0;j<WNN_MAX_FILE_OF_AN_ENV;j++){
		(w->file)[j]= get4com();
	}
	w++;
 }
 return(c);
}

/****

****/
int
js_hindo_set(env,dic,entry,ima,hindo)
struct wnn_env *env;
int dic, entry, ima,hindo;
{register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_HINDO_SET);

 put4com(dic);
 put4com(entry);
 put4com(ima);
 put4com(hindo);
 snd_flush();
 if((x=get4com())==-1){
	wnn_errorno= get4com();return -1;
 }
 return x;
}


/****
	Henkan
****/

static void
put_fzk_vec(hinsi,fzk,vec,vec1)
int hinsi;
w_char *fzk;
int vec;
int vec1;
{
	put4com(hinsi);
	putwscom(fzk);
	put4com(vec);
	put4com(vec1);
}

/**
	kanren
**/
static int rcv_dai();
static void rcv_sho_x();
static void rcv_sho_kanji();

int
js_kanren(env,yomi,hinsi,fzk,vec,vec1,vec2,rb)
struct wnn_env *env;
w_char	*yomi;
int	hinsi;
w_char	*fzk;
int	vec;
int	vec1;
int	vec2;
struct wnn_ret_buf *rb;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);

 snd_env_head(env,JS_KANREN);
 putwscom(yomi);
 put_fzk_vec(hinsi,fzk,vec,vec1);
 put4com(vec2);
 snd_flush();
 return rcv_dai(rb);
}

/*	rcv dai		*/
static int
rcv_dai(ret)
struct wnn_ret_buf *ret;
{int dai_cnt,sho_sum,kanji_sum,d_size,s_size,k_size,x;
 register int i;
 struct wnn_dai_bunsetsu *dai_list;
 register struct wnn_dai_bunsetsu *dp;
 struct wnn_sho_bunsetsu *sho_list;
 register struct wnn_sho_bunsetsu *sp;
 w_char *kanji,*kp;

 dai_cnt = get4com();
 if(dai_cnt == -1){	/* error dayo */
	wnn_errorno = get4com();
	return -1;
 }
 sho_sum = get4com();
 kanji_sum = get4com();

 d_size=sizeof(struct wnn_dai_bunsetsu)*dai_cnt;
 s_size=sizeof(struct wnn_sho_bunsetsu)*sho_sum;
 k_size=sizeof(w_char)*kanji_sum;

/* re_alloc(ret, d_size+s_size+k_size); Seems This cause Bug?? H.T.*/
 re_alloc(ret, d_size+s_size+k_size); 

 dai_list = ( struct wnn_dai_bunsetsu *) ret->buf;
 sho_list = ( struct wnn_sho_bunsetsu *)((char *)ret->buf + d_size);
 kanji = (w_char *)((char *)ret->buf + d_size + s_size);

 for(dp = dai_list,i=0; i<dai_cnt; i++){
	dp -> end = get4com();
	dp -> start = get4com();
	dp -> sbncnt = get4com();
	dp -> hyoka = get4com();
	dp++;
 }

 for(dp = dai_list, sp = sho_list, i=0; i<dai_cnt; i++){
	dp -> sbn = sp;
	x = dp -> sbncnt;
	rcv_sho_x(sp,x);
	sp += x;
	dp++;
 }

 for(dp=dai_list, kp=kanji, i=0; i<dai_cnt; i++){
	rcv_sho_kanji(dp -> sbn,dp -> sbncnt,&kp);
	dp++;
 }
 return dai_cnt;
}

/*	rcv sho routines	*/
static void
rcv_sho_x(sho_list,sho_cnt)
register struct wnn_sho_bunsetsu *sho_list;
int sho_cnt;
{register int i;
 for(i=0;i<sho_cnt;i++){
	sho_list -> end = get4com();
	sho_list -> start = get4com();
	sho_list -> jiriend = get4com();
	sho_list -> dic_no = get4com();
	sho_list -> entry = get4com();
	sho_list -> hindo = get4com();
	sho_list -> ima = get4com();
	sho_list -> hinsi = get4com();
	sho_list -> status = get4com();
	sho_list -> status_bkwd = get4com();
	sho_list ->kangovect = get4com();
	sho_list -> hyoka = get4com();
	sho_list++;
 }
}

static void
rcv_sho_kanji(sho_list,sho_cnt,kanji)
struct wnn_sho_bunsetsu *sho_list;
int sho_cnt;
w_char **kanji;
{
 register w_char *k;
 register int i, x;
 k = *kanji;
 for(i=0;i<sho_cnt;i++){
	sho_list -> kanji = k;
	getwscom(k);
	x= wnn_Strlen(k);
	k += x+1;

	sho_list -> yomi = k;
	getwscom(k);
	x= wnn_Strlen(k);
	k += x+1;

	sho_list -> fuzoku = k;
	getwscom(k);
	x= wnn_Strlen(k);
	k += x+1;
	sho_list++;
 }
 *kanji = k;
}


static int
rcv_sho(ret)
struct wnn_ret_buf *ret;
{register int sho_sum,kanji_sum,s_size,k_size;
 struct wnn_sho_bunsetsu *sho_list;
 w_char *kanji,*kp;

 sho_sum = get4com();
 if(sho_sum == -1){	/* error dayo */
	wnn_errorno = get4com();
	return -1;
 }
 kanji_sum = get4com();

 s_size=sizeof(struct wnn_sho_bunsetsu)*sho_sum;
 k_size=sizeof(w_char)*kanji_sum;

 re_alloc(ret, s_size+k_size);

 sho_list = ( struct wnn_sho_bunsetsu *)((char *)ret->buf);
 kanji = (w_char *)((char *)ret->buf + s_size);

 rcv_sho_x(sho_list,sho_sum);
 kp=kanji;
 rcv_sho_kanji(sho_list,sho_sum,&kp);
 return sho_sum;
}

/**
	kantan
**/
int
js_kantan_dai(env,yomi,hinsi,fzk,vec,vec1,rb)
struct wnn_env *env;
w_char	*yomi;
int	hinsi;
w_char	*fzk;
int	vec;
int	vec1;
struct wnn_ret_buf *rb;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);

 snd_env_head(env,JS_KANTAN_DAI);
 putwscom(yomi);
 put_fzk_vec(hinsi,fzk,vec,vec1);
 snd_flush();
 
 return rcv_dai(rb);
}

int
js_kantan_sho(env,yomi,hinsi,fzk,vec,vec1,rb)
struct wnn_env *env;
w_char	*yomi;
int	hinsi;
w_char	*fzk;
int	vec;
int	vec1;
struct wnn_ret_buf *rb;
{int sbncnt;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);

 snd_env_head(env,JS_KANTAN_SHO);
 putwscom(yomi);
 put_fzk_vec(hinsi,fzk,vec,vec1);
 snd_flush();
 
 sbncnt = rcv_sho(rb);
 return sbncnt;
}

/**
	kanzen
**/
int
js_kanzen_dai(env,yomi,hinsi,fzk,vec,vec1,rb)
struct wnn_env *env;
w_char	*yomi;
int	hinsi;
w_char	*fzk;
int	vec;
int	vec1;
struct wnn_ret_buf *rb;
{
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_KANZEN_DAI);
 putwscom(yomi);
 put_fzk_vec(hinsi,fzk,vec,vec1);
 snd_flush();

 return rcv_dai(rb);
}


int
js_kanzen_sho(env,yomi,hinsi,fzk,vec,vec1,rb)
struct wnn_env *env;
w_char	*yomi;
int	hinsi;
w_char	*fzk;
int	vec;
int	vec1;
struct wnn_ret_buf *rb;
{int sbncnt;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_KANZEN_SHO);
 putwscom(yomi);
 put_fzk_vec(hinsi,fzk,vec,vec1);
 snd_flush();

 sbncnt = rcv_sho(rb);
 return sbncnt;
}

/**	js_version		**/
int
js_version(server,serv,libv)
int *serv,*libv;
WNN_JSERVER_ID *server;
{
 set_current_js(server);
 handler_of_jserver_dead(-1);
 snd_server_head(server, JS_VERSION);
 snd_flush();
 *libv= JLIB_VERSION;
 return *serv = get4com();
}

static void
re_alloc(ret,size)
register struct wnn_ret_buf *ret;
int size;
{
 if(ret->size < size){
	if(ret->buf)
	    free((char *)ret->buf);
	ret->buf = malloc(size);
	ret->size = size;
 }
}


int
js_kill(server)
WNN_JSERVER_ID *server;
{
    int x;
    set_current_js(server);
    handler_of_jserver_dead(-1);
    snd_server_head(server, JS_KILL);
    snd_flush();
    x = get4com();
    return(x);
}


int
js_file_remove(server, n, pwd)
WNN_JSERVER_ID *server;
char *n, *pwd;
{
    register int x;
    set_current_js(server);
    handler_of_jserver_dead(-1);
    snd_server_head(server,JS_FILE_REMOVE);
    putscom(n);
    putscom(pwd);
    snd_flush();
    if((x=get4com())==-1){
	wnn_errorno= get4com();return -1;
    }
    return(x);
}    

int
js_file_remove_client(server, n, pwd)
WNN_JSERVER_ID *server;
char *n, *pwd;
{
    struct wnn_file_head fh;
    register FILE *fp;
    set_current_js(server);
    handler_of_jserver_dead(-1);
    if(js_file_loaded_local(server, n) != -1){
	wnn_errorno = WNN_FILE_IN_USE;
	return(-1);
    }
#ifdef WRITE_CHECK
    check_backup(n);
#endif /* WRITE_CHECK */
    if((fp=fopen(n,"r")) == NULL){
	wnn_errorno = WNN_FILE_READ_ERROR;
	return(-1);
    }
    if(input_file_header(fp, &fh) == -1){
	fclose(fp);
	return(-1);
    }
    fclose(fp);
    if(!check_pwd(pwd, fh.file_passwd)){
	wnn_errorno = WNN_INCORRECT_PASSWD;
	return(-1);
    }
    if(unlink(n) == -1){
	wnn_errorno = WNN_UNLINK;
	return(-1);
    }
    return(0);
}

/**	js_dic_file_create_client	**/
int
js_dic_file_create_client(env,fn,type,com,passwd, hpasswd)
struct wnn_env *env;
int type;
char *fn;
w_char *com;
char *passwd, *hpasswd;
{
    int x;
    if(type != WNN_REV_DICT && 
       type != CWNN_REV_DICT &&
       type != BWNN_REV_DICT &&
       type != WNN_UD_DICT){
	wnn_errorno = WNN_NOT_A_UD;
	return(-1);
    }
    x = create_null_dic(fn,com, passwd, hpasswd, type);
    if(x == -1){
	wnn_errorno = WNN_FILE_CREATE_ERROR;
	return(-1);
    }
    return(0);
}


/**	js_hindo_file_create_client	**/
int
js_hindo_file_create_client(env,fid,fn,com,hpasswd)
struct wnn_env *env;
int	fid;
char *fn;
w_char *com;
char *hpasswd;
{
    register int x;
    struct wnn_file_uniq funiq;
    int serial;
    register int i;

    if(env==0) return(-1);
    set_current_js(env->js_id);
    handler_of_jserver_dead(-1);
    snd_env_head(env,JS_HINDO_FILE_CREATE_CLIENT);
    put4com(fid);
    snd_flush();
    x = get4com();
    if(x == -1){
	wnn_errorno = get4com();
	return(-1);
    }
    serial = get4com();
    funiq.time = get4com();
    funiq.dev =  get4com();
    funiq.inode = get4com();
    for(i=0;i<WNN_HOSTLEN;i++){
	funiq.createhost[i]=get1com();
    }
/*    getscom(funiq.createhost); */
    if(create_hindo_file(&funiq,fn,com,hpasswd,serial) == -1){
	wnn_errorno = WNN_FILE_CREATE_ERROR;
	return(-1);
    }
    return(0);
}

int
js_file_comment_set(env, fid, comment)
struct wnn_env *env;
int fid;
w_char *comment;
{
 register int x;
 if(env==0) return(-1);
 set_current_js(env->js_id);
 handler_of_jserver_dead(-1);
 snd_env_head(env,JS_FILE_COMMENT_SET);
 put4com(fid);
 putwscom(comment);
 snd_flush();
 x=get4com();
 if(x==-1){
     wnn_errorno= get4com();
     return(-1);
 }
 return(0);
}
    

/* 
 * Hinsi Primitives
 */

int
js_hinsi_name(server, no, rb)
WNN_JSERVER_ID *server;
int no;
struct wnn_ret_buf *rb;
{
    register int size;

    set_current_js(server);
    handler_of_jserver_dead(-1);
    snd_server_head(server,JS_HINSI_NAME);
    put4com(no);
    snd_flush();
    if((size = get4com()) == -1){
	wnn_errorno = get4com(); return(-1);
    }
    re_alloc(rb,sizeof(w_char)*(size + 1));
    getwscom((w_char *)rb->buf);
    return(0);
}


int 
js_hinsi_number(server, name)
WNN_JSERVER_ID *server;
w_char *name;
{
    register int no;

    set_current_js(server);
    handler_of_jserver_dead(-1);
    snd_server_head(server,JS_HINSI_NUMBER);
    putwscom(name);
    snd_flush();
    if((no = get4com()) == -1){
	wnn_errorno = get4com();
	return(-1);
    }
    return(no);
}


int
js_hinsi_list(env, dic_no, name, rb)
struct wnn_env *env;
int dic_no;
w_char *name;
struct wnn_ret_buf *rb;
{
    int size;
    int count;
    register w_char *s;
    register w_char **r;
    register int k;

    if(env==0) return(-1);
    set_current_js(env->js_id);
    handler_of_jserver_dead(-1);
    snd_env_head(env,JS_HINSI_LIST);
    put4com(dic_no);
    putwscom(name);
    snd_flush();
    if((count = get4com()) == -1){
	wnn_errorno = get4com();
	return(-1);
    }
    size = get4com();
    re_alloc(rb,sizeof(w_char)*(size + 1) + count * sizeof(w_char *) );
    r = (w_char **)rb->buf;
    s = (w_char *)((w_char **)rb->buf + count);
    for(k = 0 ; k < count ; k++){
	*r++ = s;
	getwscom(s);
	s+= wnn_Strlen(s) + 1;
    }
    return(count);
}


int 
js_hinsi_dicts(env, no,  rb)
struct wnn_env *env;
int no;
struct wnn_ret_buf *rb;
{
    register int k, count;
    int *p;

    if(env==0) return(-1);
    set_current_js(env->js_id);
    handler_of_jserver_dead(-1);
    snd_env_head(env,JS_HINSI_DICTS);
    put4com(no);
    snd_flush();
    if((count = get4com()) == -1){
	wnn_errorno = get4com();
	return(-1);
    }
    re_alloc(rb,sizeof(int) * (count + 1));
    p = (int *)rb->buf;

    for(k = 0 ; k < count ; k++){
	*p++ = get4com();
    }
    return(count);
}


char *wnn_dic_types[] = {"","固定","登録","逆引","正規"};

char *cwnn_dic_types[] = {"","耕協","鞠村","憧咄","屎号"};
char *bwnn_dic_types[] = {"","耕協","鞠村","永侏","屎号"};

char *kwnn_dic_types[] = {"","壱舛","去系","蝕遂","舛鋭"};
			/*    由鑷   竒巵   羹齎   閨豫 */

/* New primitives  9/8 */

int
js_file_password_set(env, fid, which, old, new)
struct wnn_env *env;
int fid;
int which;			/* 1 for dic, 2 for hindo 3(0) for both*/
char *old, *new;
{
    register int x;
    if(env==0) return(-1);
    set_current_js(env->js_id);
    handler_of_jserver_dead(-1);
    snd_env_head(env,JS_FILE_PASSWORD_SET);
    put4com(fid);
    put4com(which);
    putscom(old);
    putscom(new);
    snd_flush();
    x=get4com();
    if(x==-1){
	wnn_errorno= get4com();
	return(-1);
    }
    return(0);
}

int
js_hinsi_table_set(env, dic_no, hinsi_table)
struct wnn_env *env;
int dic_no;
w_char *hinsi_table;
{
    int x;
    if(env==0) return(-1);
    set_current_js(env->js_id);
    handler_of_jserver_dead(-1);
    snd_env_head(env,JS_HINSI_TABLE_SET);
    put4com(dic_no);
    putwscom(hinsi_table);
    snd_flush();
    x=get4com();
    if(x==-1){
	wnn_errorno= get4com();
	return(-1);
    }
    return(0);
}

#define	SERVERDEFS_FILE		"/serverdefs"

#define	MACHINE_NAME		1
#define	UNIXDOMAIN_NAME		2
#define	SERVICE_NAME		3
#define	SERVICE_PORT_NUM	4

static char *
get_serv_defs(lang, cnt)
char *lang;
int cnt;
{
    FILE *fp;
    static char s[6][EXPAND_PATH_LENGTH];
    char serv_defs[EXPAND_PATH_LENGTH];
    char data[1024];
    int num;

    strcpy(serv_defs, LIBDIR);
    strcat(serv_defs, SERVERDEFS_FILE);
    if ((fp = fopen(serv_defs, "r")) == NULL) {
	return(NULL);
    }
    while (fgets(data, 1024, fp) != NULL) {
	num = sscanf(data, "%s %s %s %s %s %s %s",
		     s[0],s[1],s[2],s[3],s[4],s[5],s[6]);
	if ((num < 4) || s[0][0] == ';') { 
	    continue;
	}
	if (!strncmp(lang, s[0], strlen(s[0]))) {
	    fclose(fp);
	    if (cnt == SERVICE_PORT_NUM && num < 5) return(NULL);
	    if (strlen(s[cnt]) == strlen("NULL") && !strcmp(s[cnt], "NULL")) {
		return(NULL);
	    } else {
		return(s[cnt]);
	    }
	}
    }
    fclose(fp);
    return(NULL);
}

char *
_wnn_get_machine_of_serv_defs(lang)
char *lang;
{
    return(get_serv_defs(lang, MACHINE_NAME));
}

static char *
get_unixdomain_of_serv_defs(lang)
char *lang;
{
    return(get_serv_defs(lang, UNIXDOMAIN_NAME));
}

static char *
get_service_of_serv_defs(lang)
char *lang;
{
    return(get_serv_defs(lang, SERVICE_NAME));
}

static int
get_port_num_of_serv_defs(lang)
char *lang;
{
    char *port_char;

    if ((port_char = get_serv_defs(lang, SERVICE_PORT_NUM)) == NULL) {
	return(-1);
    } else {
	return(atoi(port_char));
    }
}
