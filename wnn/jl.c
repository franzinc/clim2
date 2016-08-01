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
	Nihongo Henkan Hi-level Library
*/

#include "commonhd.h"
#include "config.h"
#if defined(__AARCH64EL__)
#include <bits/types.h>
#undef __FD_SETSIZE
#define __FD_SETSIZE 65536
#include <stdlib.h>
#endif
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>  /* for F_OK */
#ifdef SYSVR2
#   include <string.h>
#endif
#ifdef BSD42
#   include <strings.h>
#endif
#include <sys/file.h>
#include <ctype.h>
#include <pwd.h>

#include "wnnerror.h"
#include "jd_sock.h"
#include "jslib.h"
#include "jllib.h"

#include "msg.h"
#include "wnn_string.h"
extern struct msg_cat *wnn_msg_cat;


#define MAXENVS 32

#define MAXINCLUDE 10

#define DEFAULT_BUN_LEN 3

#define DEFAULT_ZENKOUHO_LEN 3

#define DEFAULT_HEAP_LEN 3

#define INCREMENT 2

#define SHO 0
#define DAI 1

#define BUN 0
#define ZENKOUHO 1   /* Must not change!, they are assigned to two bit flag */
#define ZENKOUHO_DAI 3 /* DAI << 1 | ZENKOUHO */

#define	if_dead_disconnect(env, ret) \
{ \
	if (wnn_errorno == WNN_JSERVER_DEAD) { \
		jl_disconnect_if_server_dead(env);\
		return(ret); \
	} else { \
		return(ret); \
	} \
}

#define	if_dead_disconnect_b(buf, ret) \
{ \
	if (wnn_errorno == WNN_JSERVER_DEAD) { \
		jl_disconnect_if_server_dead(buf->env);\
		buf->env = 0; \
		return(ret); \
	} else { \
		return(ret); \
	} \
}

static struct wnn_ret_buf rb = {0, NULL};
static struct wnn_ret_buf dicrb = {0, NULL};
static struct wnn_ret_buf wordrb = {0, NULL};

static int dumbhinsi;
static w_char *mae_fzk;
static int syuutanv;
static int syuutanv1;

#define CONFIRM  1
#define CONFIRM1 2
#define CREATE_WITHOUT_CONFIRM  3
#define NO_CREATE 4

int confirm_state;

static void add_down_bnst();
static int alloc_heap();
static int call_error_handler();
static int change_ascii_to_int();
static int create_file();
static int dai_end();
static int expand_expr();
static int expand_expr_all();
static int file_discard();
static int file_exist();
static int file_read();
static int file_remove();
static int find_same_kouho();
static int find_same_kouho_dai();
static void free_bun();
static void free_down();
static void free_sho();
static void free_zenkouho();
static int get_c_jikouho();
static int get_c_jikouho_dai();
static int get_c_jikouho_from_zenkouho();
static int get_c_jikouho_from_zenkouho_dai();
static int get_pwd();
static int insert_dai();
static int insert_sho();
static int make_dir1();
static int make_dir_rec1();
static void make_space_for();
static void make_space_for_bun();
static void make_space_for_zenkouho();
static void message_out();
static int ren_conv1();
static void set_dai();
static void set_sho();
static int tan_conv1();

/*
 * Sub-routines to handle files, enviroments and connections.
 */

struct wnn_file_name_id {
    struct wnn_file_name_id *next;
    int id;
    char name[1];
};

struct wnn_jl_env{
    WNN_JSERVER_ID *js;
    struct wnn_env *env;
    char env_n[WNN_ENVNAME_LEN];
    char server_n[WNN_HOSTLEN];
    char lang[32];
    int ref_cnt;
    struct wnn_file_name_id *file;
} envs[MAXENVS];

/* 
 * File management routines.
 */

static 
struct wnn_jl_env *find_jl_env(env)
register struct wnn_env *env;
{
    register int k;
    for(k = 0 ; k < MAXENVS; k++){
	if(envs[k].env == env) return(envs + k);
    }
    return(NULL);
}


static
struct wnn_env *
find_env_of_same_js_id(js_id)
register WNN_JSERVER_ID *js_id;
{
    register int k;

    for(k = 0 ; k < MAXENVS; k++){
	if (envs[k].js == js_id){
	    return(envs[k].env);
	}
    }
    return(NULL);
}

static
WNN_JSERVER_ID *
find_same_server(server_n, lang)
register char *server_n, *lang;
{
    register int k;

    if (server_n == NULL || lang == NULL) return(NULL);
    for(k = 0 ; k < MAXENVS; k++){
	if(strncmp(envs[k].server_n, server_n, WNN_HOSTLEN - 1) == 0 &&
	   strcmp(envs[k].lang, lang) == 0){
	    return(envs[k].js);
	}
    }
    return(NULL);
}

static int
find_same_server_from_id(js)
register WNN_JSERVER_ID *js;
{
    register int k;
    for(k = 0 ; k < MAXENVS; k++){
	if(envs[k].js == js) return(1);
    }
    return(0);
}

#ifdef nodef
static
delete_server_from_id(js)
WNN_JSERVER_ID *js;
{
    int k;
    for(k = 0 ; k < MAXENVS; k++){
	if(envs[k].js == js) {
		envs[k].js = 0;
		strcpy(envs[k].server_n, "");
		break;
	}
    }
}
#endif


static
struct wnn_env *
find_same_env(js, env_n, lang)
register WNN_JSERVER_ID *js;
register char *env_n;
char *lang;
{
    register int k;

    if (env_n == NULL || lang == NULL) return(NULL);
    for(k = 0 ; k < MAXENVS; k++){
	if(envs[k].js == js && strcmp(envs[k].env_n, env_n) == 0
			    && strcmp(envs[k].lang, lang) == 0){
	    envs[k].ref_cnt++;
	    return(envs[k].env);
	}
    }
    return(NULL);
}

static char *
env_name(env)
register struct wnn_env *env;
{
    register int k;

    for(k = 0 ; k < MAXENVS; k++){
	if(envs[k].env == env){
	    return(envs[k].env_n);
	}
    }
    return(NULL);
}


static void
add_new_env(js, env, env_n, server_n, lang)
register WNN_JSERVER_ID *js;
register struct wnn_env *env;
char *env_n, *server_n, *lang;
{
    register int k;

    for(k = 0 ; k < MAXENVS; k++){
	if(envs[k].ref_cnt == 0){
	    strncpy(envs[k].server_n, server_n, WNN_HOSTLEN - 1);
	    envs[k].server_n[WNN_HOSTLEN - 1] = '\0';
	    strcpy(envs[k].env_n, env_n);
	    strcpy(envs[k].lang, lang);
	    envs[k].js = js;
	    envs[k].env = env;
	    envs[k].ref_cnt = 1;
	    envs[k].file = NULL;
	    break;
	}
    }
}

static int
delete_env(env)
register struct wnn_env *env;
{
    register int k;

    for(k = 0 ; k < MAXENVS; k++){
	if(envs[k].env == env){
	    if(--envs[k].ref_cnt == 0){
		strcpy(envs[k].server_n, "");
		strcpy(envs[k].env_n, "");
		strcpy(envs[k].lang, "");
		envs[k].js = NULL;
		envs[k].env = NULL;
		return(1);	/* Need To delete env */
	    }else{
		return(0);	/* Need not to delete env */
	    }
	}
    }
    return(-1);			/* This must not happen */
}


/* Routines to manipulate files */

static int
add_file_to_env(env, id, name)
struct wnn_env *env;
int id;
register char *name;
{
    register struct wnn_file_name_id *f, *f1;
    if((f = (struct wnn_file_name_id *)malloc(sizeof(struct wnn_file_name_id) +
					     strlen(name) + 1)) == NULL){
	wnn_errorno=WNN_ALLOC_FAIL;	return(-1);
    }
    strcpy(f->name, name);
    f->id = id;
    f1 = find_jl_env(env)->file;
    f->next = f1;
    find_jl_env(env)->file = f;
    return(0);
}
    
static char *
find_file_name_from_id(env, id)
struct wnn_env *env;
register int id;
{
    register struct wnn_file_name_id *f;
    f = find_jl_env(env)->file;
    for(;f; f = f->next){
	if(f->id == id){
	    return(f->name);
	}
    }
/*    wnn_errorno=WNN_FILE_NOT_READ_FROM_CLIENT; */
    return(NULL);
}

static int
delete_file_from_env(env, id)
struct wnn_env *env;
register int id;
{
    struct wnn_file_name_id *f;
    register struct wnn_file_name_id **prev;
    register struct wnn_jl_env *jl_env_p;

    jl_env_p = find_jl_env(env);
    if (!jl_env_p->file) return(0);
    for(prev = &jl_env_p->file; f = *prev; prev = &f->next){
	if (f->id == id) {
	    *prev = f->next;
	    free(f);
	    return(0);
	}
    }
    wnn_errorno=WNN_FILE_NOT_READ_FROM_CLIENT;
    return(-1);
}

/*
 * Libraries which handle Connection To Jserver
 */

struct wnn_env *
jl_connect_lang(env_n, server_n, lang, wnnrc_n, error_handler, message_handler, timeout)
register char *env_n, *server_n, *wnnrc_n, *lang;
int  (*error_handler)(), (*message_handler)();
int timeout;
{
    register WNN_JSERVER_ID	*js = NULL;
    struct wnn_env *env;
    int env_exist;
    char p_lang[16];
    register char *p, *l;
    extern char *getenv();
    extern char *_wnn_get_machine_of_serv_defs();

    wnn_errorno = 0;
    /* if lang not specified use $LANG */
    if(!lang || !*lang){
	lang = getenv("LANG");
    }
    if (!lang || !*lang){
/* Sorry! Default is Japanese. :-) */
	strcpy(p_lang, "ja_JP");
    } else {
	/* Use only [language]_[teritorry] */
	for(p = p_lang, l = lang; *l != '@' && *l != '.' && *l != 0; p++, l++)
	    *p = *l;
	*p = 0;
    }

   /* To See serverdefs file when server_n is not specified. */
    if(!server_n || !*server_n){
	/* find server machine name from table by lang */
	if (server_n = _wnn_get_machine_of_serv_defs(p_lang)) {
	    if((js = find_same_server(server_n, p_lang)) == NULL){
		if((js = js_open_lang(server_n, p_lang, timeout)) == NULL){
		    server_n = NULL;
		}
	    }
	}
	if (!server_n || !*server_n) {
	    server_n = "unix";
	}
    }

    if (js == NULL) {
	if((js = find_same_server(server_n, p_lang)) == NULL){
	    if((js = js_open_lang(server_n, p_lang, timeout)) == NULL){
		return(NULL);
	    }
    /*	js_hinsi_list(js); */
	}
    }
    if ((env_exist = js_env_exist(js, env_n)) < 0)
	return (NULL);
    if((env = find_same_env(js, env_n, p_lang)) == NULL){ /* Incr ref_cnt */
	if((env = js_connect_lang(js, env_n, p_lang)) == NULL){
	    return(NULL);
	}
	add_new_env(js, env, env_n, server_n, p_lang);
    }
    if(env_exist == 0 && wnnrc_n){
	jl_set_env_wnnrc(env, wnnrc_n, error_handler, message_handler);
    }
    return(env);
}

void
jl_disconnect(env)
register struct wnn_env *env;
{
    int ret;
    wnn_errorno = 0;
    if((ret = delete_env(env)) < 0){
	return;
    } else if (ret) {
	js_disconnect(env);
    }
    if(!find_same_server_from_id(env->js_id)){
	js_close(env->js_id);
	env->js_id = 0;
    }
}

int
jl_isconnect_e(env)
register struct wnn_env *env;
{
    if (js_isconnect(env) == 0)
	return(1);
    else
	return (0);
}

/* JSERVER が死んだら env を disconnect して回る */
void
jl_disconnect_if_server_dead(env)
register struct wnn_env *env;
{
    register struct wnn_env *same_env;
    int ret;

    if((ret = delete_env(env)) < 0){
	return;
    } else if (ret) {
	js_disconnect(env);
    }
    while ((same_env = find_env_of_same_js_id(env->js_id)) != 0) {
	if(delete_env(same_env)){
	    js_disconnect(same_env);
	}
	
    }
    js_close(env->js_id);
    env->js_id = 0;
}


struct wnn_buf *
jl_open_lang(env_n, server_n, lang, wnnrc_n, error_handler, message_handler, timeout)
char *env_n, *server_n, *wnnrc_n, *lang;
int  (*error_handler)(), (*message_handler)();
int timeout;
{
    register int k, dmy;
    register struct wnn_buf *buf;
    struct wnn_env *env;

    wnn_errorno = 0;
    if(rb.size == 0) rb.buf = (char *)malloc((unsigned)(rb.size = 1024));

#define ALLOC_SET(pter, type, size, size_var) \
    ((pter) = ((type *)malloc((unsigned)(sizeof(type) * ((size_var) = (size))))))

    if(!ALLOC_SET(buf, struct wnn_buf, 1, dmy)){
	wnn_errorno=WNN_ALLOC_FAIL;return NULL;
    }

    buf->bun_suu = 0;
    buf->zenkouho_suu = 0;
    buf->zenkouho_daip = 0;
    buf->c_zenkouho = -1;
    buf->zenkouho_bun = -1;
    buf->zenkouho_end_bun = -1;
    buf->free_heap = NULL;
    buf->heap = NULL;
    buf->zenkouho_dai_suu = 0;
    
    if(!ALLOC_SET(buf->bun, WNN_BUN *, DEFAULT_BUN_LEN, buf->msize_bun) ||
       !ALLOC_SET(buf->zenkouho_dai, int, DEFAULT_ZENKOUHO_LEN + 1, buf->msize_zenkouho) ||
       !ALLOC_SET(buf->zenkouho, WNN_BUN *, DEFAULT_ZENKOUHO_LEN, buf->msize_zenkouho) ||
       !ALLOC_SET(buf->down_bnst, WNN_BUN *, DEFAULT_BUN_LEN, buf->msize_bun)
       ){
	wnn_errorno=WNN_ALLOC_FAIL;return NULL;
    }
       
    for(k = 0 ; k < DEFAULT_BUN_LEN ; k++){
	buf->down_bnst[k] = NULL;
    }

    if(alloc_heap(buf, DEFAULT_HEAP_LEN) == -1){
	return NULL;
    }

    env = jl_connect_lang(env_n, server_n, lang, wnnrc_n,
			error_handler, message_handler, timeout);
    buf->env = env;
    return(buf);
}
    

static int
alloc_heap(buf, len)
struct wnn_buf *buf;
register int len;
{
    char **c;
    register WNN_BUN *d;

    if((c =(char **)malloc((unsigned)(len * sizeof(WNN_BUN) + sizeof(char *)))) == NULL){
	wnn_errorno=WNN_ALLOC_FAIL;	return(-1);
    }

    *c = buf->heap;
    buf->heap = (char *)c;
    d = (WNN_BUN *)(c + 1);
    for(--len; len > 0 ; len--, d++){
	d->free_next = d + 1;
    }
    d->free_next = buf->free_heap;
    buf->free_heap = (WNN_BUN *)(c + 1);
    return(0);
}

void
jl_close(buf)
register struct wnn_buf *buf;
{
    register char *c, *next;
    
    wnn_errorno = 0;
    if(buf == NULL) return;

    if(buf->env){
	jl_disconnect(buf->env);
	buf->env = 0;
    }

    if(buf->bun) free((char *)buf->bun);
    if(buf->zenkouho) free((char *)buf->zenkouho);
    if(buf->zenkouho_dai) free((char *)buf->zenkouho_dai);
    if(buf->down_bnst) free((char *)buf->down_bnst);
    for(c = buf->heap; c; c = next) {
	next = *(char **)c;
	free(c);
    }
    free((char *)buf);
}

/*
 * Conversion Libraries 
 */

int
jl_ren_conv(buf, yomi, bun_no, bun_no2, use_maep)
register struct wnn_buf *buf;
register w_char *yomi;
int bun_no, bun_no2;
int use_maep;
{
    wnn_errorno = 0;
    if(bun_no < 0) return(-1);
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0) bun_no2 = buf->bun_suu;
    free_down(buf, bun_no, bun_no2);
    return(ren_conv1(buf, yomi, bun_no, bun_no2, use_maep));
}

static int
ren_conv1(buf, yomi, bun_no, bun_no2, use_maep)
register struct wnn_buf *buf;
w_char *yomi;
register int bun_no, bun_no2;
int use_maep;
{
    int dcnt;
    struct wnn_dai_bunsetsu *dp;
    int size;
    w_char yomi1[LENGTHBUNSETSU];


    if (yomi == NULL || *yomi == (w_char)0) return(0);
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0) bun_no2 = buf->bun_suu;

    if (use_maep & WNN_USE_MAE && bun_no > 0) {
	dumbhinsi = buf->bun[bun_no - 1]->hinsi;
	jl_get_yomi(buf, bun_no - 1, bun_no, yomi1);
	mae_fzk = yomi1 + buf->bun[bun_no - 1]->jirilen;
    } else {
	dumbhinsi = WNN_BUN_SENTOU;
	mae_fzk = (w_char *)0;
    }
    if(use_maep & WNN_USE_ATO && bun_no2 < buf->bun_suu){
	syuutanv = buf->bun[bun_no2]->kangovect;
	syuutanv1 = WNN_VECT_KANREN;
    }else{
	syuutanv = WNN_VECT_KANREN;
	syuutanv1 = WNN_VECT_NO;
	if(bun_no2 < buf->bun_suu){
	    buf->bun[bun_no2]->dai_top = 1;
	}
    }
    if((dcnt = js_kanren(buf->env, yomi, dumbhinsi, mae_fzk,
			 syuutanv, syuutanv1, WNN_VECT_BUNSETSU, &rb)) < 0){
	if_dead_disconnect_b(buf, -1);
    }

    dp = (struct wnn_dai_bunsetsu *)rb.buf;

    free_bun(buf, bun_no, bun_no2);

    if(use_maep & WNN_USE_ATO && bun_no2 < buf->bun_suu){
	buf->bun[bun_no2]->dai_top =
	    (dp[dcnt-1].sbn[dp[dcnt-1].sbncnt-1].status_bkwd == WNN_CONNECT_BK)? 0:1;
    }

    size = insert_dai(buf, BUN, bun_no, bun_no2, dp, dcnt, 0);
    if(buf->zenkouho_end_bun > bun_no && buf->zenkouho_bun < bun_no2){
	free_zenkouho(buf);
    }else if(buf->zenkouho_bun >= bun_no2){
	buf->zenkouho_bun += size - bun_no2;
	buf->zenkouho_end_bun += size - bun_no2;

    }	
    return(buf->bun_suu);
}

int
jl_tan_conv(buf, yomi, bun_no, bun_no2, use_maep, ich_shop)
register struct wnn_buf *buf;
w_char *yomi;
register int bun_no, bun_no2;
int use_maep, ich_shop;
{
    wnn_errorno = 0;
    if(bun_no < 0) return(-1);
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0) bun_no2 = buf->bun_suu;
    free_down(buf, bun_no, bun_no2);
    if(tan_conv1(buf, yomi, bun_no, bun_no2, use_maep, ich_shop) == -1)
	return(-1);
    return(buf->bun_suu);
}

static int
tan_conv1(buf, yomi, bun_no, bun_no2, use_maep, ich_shop)
register struct wnn_buf *buf;
w_char *yomi;
register int bun_no, bun_no2;
int use_maep, ich_shop;
{
    int dcnt;
    struct wnn_dai_bunsetsu *dp;
    struct wnn_sho_bunsetsu *sp;
    int ret;
    w_char yomi1[LENGTHBUNSETSU];

    if (yomi == NULL || *yomi == (w_char)0) return(0);
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0) bun_no2 = buf->bun_suu;

    if (use_maep & WNN_USE_MAE && bun_no > 0) {
	dumbhinsi = buf->bun[bun_no - 1]->hinsi;
	jl_get_yomi(buf, bun_no - 1, bun_no, yomi1);
	mae_fzk = yomi1 + buf->bun[bun_no - 1]->jirilen;
    } else {
	dumbhinsi = WNN_BUN_SENTOU;
	mae_fzk = (w_char *)0;
    }
    if(use_maep & WNN_USE_ATO && bun_no2 < buf->bun_suu){
	syuutanv = buf->bun[bun_no2]->kangovect;
	syuutanv1 = WNN_VECT_KANTAN;
    }else{
	syuutanv = WNN_VECT_KANTAN;
	syuutanv1 = WNN_VECT_NO;
	if(bun_no2 < buf->bun_suu){
	    buf->bun[bun_no2]->dai_top = 1;
	}
    }
    if(ich_shop == WNN_SHO){
	if((dcnt = js_kantan_sho(buf->env, yomi, dumbhinsi, mae_fzk,
				 syuutanv, syuutanv1, &rb)) < 0){
	    if_dead_disconnect_b(buf, -1);
	}
	sp = (struct wnn_sho_bunsetsu *)rb.buf;
	if(use_maep & WNN_USE_ATO && bun_no2 < buf->bun_suu){
	    buf->bun[bun_no2]->dai_top =
		(sp[dcnt-1].status_bkwd == WNN_CONNECT_BK)? 0:1;
	}
	free_bun(buf, bun_no, bun_no2);
	ret = insert_sho(buf, BUN, bun_no, bun_no2, sp, dcnt, 0);
    }else{
	if((dcnt = js_kantan_dai(buf->env, yomi,  dumbhinsi, mae_fzk,
				 syuutanv, syuutanv1, &rb)) < 0){
	    if_dead_disconnect_b(buf, -1);
	}
	dp = (struct wnn_dai_bunsetsu *)rb.buf;
	if(use_maep & WNN_USE_ATO && bun_no2 < buf->bun_suu){
	    buf->bun[bun_no2]->dai_top =
		(dp[dcnt-1].sbn[dp[dcnt-1].sbncnt-1].status_bkwd == WNN_CONNECT_BK)? 0:1;
	}
	free_bun(buf, bun_no, bun_no2);
	ret = insert_dai(buf, BUN, bun_no, bun_no2, dp, dcnt, 0);
    }
    if(buf->zenkouho_end_bun > bun_no && buf->zenkouho_bun < bun_no2){
	free_zenkouho(buf);
    }else if(buf->zenkouho_bun >= bun_no2){
	buf->zenkouho_bun += ret - bun_no2;
	buf->zenkouho_end_bun += ret - bun_no2;
    }	
    return(ret);
}

int
jl_nobi_conv(buf, bun_no, ichbn_len, bun_no2, use_maep, ich_shop)
register struct wnn_buf *buf;
int ichbn_len, use_maep, ich_shop;
register int bun_no, bun_no2;
{
    w_char yomi[LENGTHCONV], ytmp;
    int ret;
    int len1;

    register WNN_BUN *b1;	/* 学習がうまくいくように変更しました。H.T. */

    wnn_errorno = 0;
    if(bun_no < 0) return(-1);
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0) bun_no2 = buf->bun_suu;
    
    len1 = jl_get_yomi(buf, bun_no, bun_no2, yomi);
    ytmp = yomi[ichbn_len];
    if(len1 < ichbn_len){
	ichbn_len = len1;
    }
    yomi[ichbn_len] = 0;

    if(buf->bun[bun_no]->nobi_top != 1){  /* need to add down_bnst */
	if(buf->bun[bun_no]) add_down_bnst(buf, bun_no, buf->bun[bun_no]);
	if(bun_no + 1 < buf->bun_suu){
	    add_down_bnst(buf, bun_no, buf->bun[bun_no + 1]);
			/* 全て bun_no の down_bnst に加えるように変更 */
	}
    }
    b1 = buf->down_bnst[bun_no];
    buf->down_bnst[bun_no] = NULL;
    free_down(buf, bun_no, bun_no2);

    if((ret = tan_conv1(buf, yomi, bun_no, bun_no2, use_maep & WNN_USE_MAE, ich_shop)) == -1){
	return(-1);
    }
    yomi[ichbn_len] = ytmp;    
    if(ytmp){
	int maep;
	if(ich_shop){
	    maep = use_maep & ~WNN_USE_MAE;
	}else{
	    maep = use_maep | WNN_USE_MAE;
	}
	if(ren_conv1(buf, yomi + ichbn_len, ret, ret, maep) == -1){
	    return(-1);
	}
    }
    buf->bun[bun_no]->nobi_top = 1;
    buf->down_bnst[bun_no] = b1;

    return(buf->bun_suu);
}

int
jl_nobi_conv_e2(buf, env, bun_no, ichbn_len, bun_no2, use_maep, ich_shop)
register struct wnn_buf *buf;
struct wnn_env *env;
int ichbn_len, use_maep, ich_shop;
register int bun_no, bun_no2;
{
    w_char yomi[LENGTHCONV], ytmp;
    int ret;
    int len1;

    wnn_errorno = 0;
    if(bun_no < 0) return(-1);
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0) bun_no2 = buf->bun_suu;
    
    len1 = jl_get_yomi(buf, bun_no, bun_no2, yomi);
    ytmp = yomi[ichbn_len];
    if(len1 < ichbn_len){
	ichbn_len = len1;
    }
    yomi[ichbn_len] = 0;

    if(buf->bun[bun_no]->nobi_top != 1){  /* need to add down_bnst */
	if(buf->bun[bun_no]) add_down_bnst(buf, bun_no, buf->bun[bun_no]);
	if(bun_no + 1 < buf->bun_suu){
	    if(ichbn_len < jl_yomi_len(buf, bun_no, bun_no + 1)){
		add_down_bnst(buf, bun_no + 1, buf->bun[bun_no + 1]);
		free_down(buf, bun_no + 2, bun_no2);
	    }else{
		add_down_bnst(buf, bun_no, buf->bun[bun_no + 1]);
		free_down(buf, bun_no + 1, bun_no2);
	    }
	}
    }

    if((ret = tan_conv1(buf, yomi, bun_no, bun_no2, use_maep & WNN_USE_MAE, ich_shop)) == -1){
	return(-1);
    }

    buf->env = env;

    yomi[ichbn_len] = ytmp;    
    if(ytmp){
	int maep;
	if(ich_shop){
	    maep = use_maep & ~WNN_USE_MAE;
	}else{
	    maep = use_maep | WNN_USE_MAE;
	}
	if(ren_conv1(buf, yomi + ichbn_len, ret, ret, maep) == -1){
	    return(-1);
	}
    }
    buf->bun[bun_no]->nobi_top = 1;

    return(buf->bun_suu);
}

int
jl_kill(buf, bun_no, bun_no2)
struct wnn_buf *buf;
register int bun_no, bun_no2;
{
    wnn_errorno = 0;
    if(bun_no < 0) return(0);
    if(bun_no2 < bun_no || bun_no2 < 0) bun_no2 = buf->bun_suu;
    free_zenkouho(buf);     /* toriaezu */
    free_down(buf, bun_no, bun_no2);
    free_bun(buf, bun_no, bun_no2);
    bcopy((char *)&buf->bun[bun_no2], (char *)&buf->bun[bun_no],
	 (buf->bun_suu - bun_no2) * sizeof(WNN_BUN *));
    bcopy((char *)&buf->down_bnst[bun_no2], (char *)&buf->down_bnst[bun_no],
	 (buf->bun_suu - bun_no2) * sizeof(WNN_BUN *));
    buf->bun_suu -= bun_no2 - bun_no;
    return(buf->bun_suu);
}

int
jl_zenkouho(buf, bun_no, use_maep, uniq_level)
register struct wnn_buf *buf;
int bun_no, use_maep, uniq_level;
{
    int cnt;
    w_char yomi[LENGTHBUNSETSU], yomi1[LENGTHBUNSETSU];
    struct wnn_sho_bunsetsu *sp;
    register int k;

    wnn_errorno = 0;
    jl_get_yomi(buf, bun_no, bun_no + 1, yomi);

    if(bun_no == buf->zenkouho_bun &&
       buf->zenkouho_daip == SHO) return(buf->c_zenkouho);
    if (use_maep & WNN_USE_MAE && bun_no > 0) {
	dumbhinsi = buf->bun[bun_no - 1]->hinsi;
	jl_get_yomi(buf, bun_no - 1, bun_no, yomi1);
	mae_fzk = yomi1 + buf->bun[bun_no - 1]->jirilen;
    } else {
	dumbhinsi = WNN_BUN_SENTOU;
	mae_fzk = (w_char *)0;
    }
    if(use_maep & WNN_USE_ATO && bun_no + 1 < buf->bun_suu){

	syuutanv = buf->bun[bun_no+1]->kangovect;
	syuutanv1 = WNN_VECT_KANZEN;
	buf->zenkouho_endvect = syuutanv;
    }else{
	syuutanv = WNN_VECT_KANZEN;
	syuutanv1 = WNN_VECT_NO;
	if(bun_no + 1 < buf->bun_suu){
	    buf->bun[bun_no + 1]->dai_top = 1;
	}
	buf->zenkouho_endvect = -1;
    }
    if((cnt = js_kanzen_sho(buf->env, yomi, dumbhinsi, mae_fzk,
			    syuutanv, syuutanv1, &rb)) < 0){
	    if_dead_disconnect_b(buf, -1);
    }

    sp = (struct wnn_sho_bunsetsu *)rb.buf;
    free_zenkouho(buf);
    if((buf->bun[bun_no]->from_zenkouho & 1) == BUN){
	set_sho(buf->bun[bun_no], &buf->zenkouho[0]);
	buf->zenkouho_suu = 1;
				/* Connection information of Old bunsetsu
				 * May not be correct.
				 */
	k = get_c_jikouho(sp, cnt, buf->bun[bun_no]);
	if(k >= 0){
	    buf->zenkouho[0]->dai_top = (sp[k].status == WNN_CONNECT)? 0:1;
	    buf->zenkouho[0]->dai_end = (sp[k].status_bkwd == WNN_CONNECT_BK)? 0:1;
	}
	if(uniq_level || k < 0){
	    insert_sho(buf, ZENKOUHO, -1, -1, sp, cnt, uniq_level);
	}else{
	    insert_sho(buf, ZENKOUHO, -1, -1, sp, k, uniq_level);
	    insert_sho(buf, ZENKOUHO, -1, -1, sp + k + 1, cnt - k - 1, uniq_level);
	}
	buf->c_zenkouho = 0;
    }else{
	insert_sho(buf, ZENKOUHO, -1, -1, sp, cnt, uniq_level);
	k = get_c_jikouho_from_zenkouho(buf, buf->bun[bun_no]);
	if(k < 0){
	    k = 0;	/* Only when the kouho has been removed from dict. */
	}
	buf->c_zenkouho = k;
    }
    buf->zenkouho_bun = bun_no;
    buf->zenkouho_end_bun = bun_no + 1;
    buf->zenkouho_daip = SHO;
    for(k = 0 ; k < buf->zenkouho_suu; k++){
	if(buf->zenkouho[k]->ima && buf->zenkouho[k]->dic_no != -1){
	    add_down_bnst(buf, bun_no, buf->zenkouho[k]);
	}
    }
    return(buf->c_zenkouho);
}

int
jl_zenkouho_dai(buf, bun_no, bun_no2, use_maep, uniq_level)
register struct wnn_buf *buf;
int bun_no, bun_no2, use_maep, uniq_level;
{
    int cnt;
    w_char yomi[LENGTHBUNSETSU], yomi1[LENGTHBUNSETSU];
    struct wnn_dai_bunsetsu *dp;
    int tmp;
    register int k;

    wnn_errorno = 0;
    if(bun_no2 > (tmp = dai_end(buf, bun_no)) ||
	bun_no2 < 0) bun_no2 = tmp;
    jl_get_yomi(buf,bun_no, bun_no2, yomi);

    if(bun_no == buf->zenkouho_bun && buf->zenkouho_daip == DAI){
	return(buf->c_zenkouho);
    }
    if (use_maep & WNN_USE_MAE && bun_no > 0) {
	dumbhinsi = buf->bun[bun_no - 1]->hinsi;
	jl_get_yomi(buf, bun_no - 1, bun_no, yomi1);
	mae_fzk = yomi1 + buf->bun[bun_no - 1]->jirilen;
    } else {
	dumbhinsi = WNN_BUN_SENTOU;
	mae_fzk = (w_char *)0;
    }
    if(use_maep & WNN_USE_ATO && bun_no2 < buf->bun_suu){
	syuutanv = buf->bun[bun_no2]->kangovect;
	syuutanv1 = WNN_VECT_KANZEN; 
	buf->zenkouho_endvect = syuutanv;
    }else{
	syuutanv = WNN_VECT_KANZEN;
	syuutanv1 = WNN_VECT_NO;
	if(bun_no2 < buf->bun_suu){
	    buf->bun[bun_no2]->dai_top = 1;
	}
	buf->zenkouho_endvect = -1;
    }
    if((cnt = js_kanzen_dai(buf->env, yomi, dumbhinsi, mae_fzk,
			    syuutanv, syuutanv1, &rb)) < 0){
	if_dead_disconnect_b(buf, -1);
    }
    dp = (struct wnn_dai_bunsetsu *)rb.buf;

    free_zenkouho(buf);
		/* Wander if it is OK, that is, only when all the
		 * zenkouho's are got from zenkouho_dai, we need not move
		 * the current dai-bunsetsu to the top of zenkouho's
		 */
    for(k = bun_no; k < bun_no2; k++){
	if(buf->bun[k]->from_zenkouho != ZENKOUHO_DAI)break;
    }
    if(k != bun_no2){		/* move the current to the top. */
	make_space_for(buf, ZENKOUHO, buf->zenkouho_suu, buf->zenkouho_suu, bun_no2 - bun_no);
	set_dai(&buf->bun[bun_no], &buf->zenkouho[0], bun_no2 - bun_no);
	buf->zenkouho_dai[0] = 0;
	buf->zenkouho_dai[1] = bun_no2 - bun_no;
	buf->zenkouho_dai_suu = 1;
	buf->zenkouho_suu = bun_no2 - bun_no;
	k = get_c_jikouho_dai(dp, cnt, buf->bun, bun_no);
	if(k >= 0){
	    buf->zenkouho[0]->dai_top = 
		(dp[k].sbn->status == WNN_CONNECT)? 0:1;
	    buf->zenkouho[bun_no2-bun_no-1]->dai_end = 
		(dp[k].sbn[dp[k].sbncnt-1].status_bkwd == WNN_CONNECT_BK)? 0:1;
	    /* KURI *//* USO*?*/
	}
	if(uniq_level || k < 0){
	    insert_dai(buf, ZENKOUHO, -1, -1, dp, cnt, uniq_level);
	}else{
	    insert_dai(buf, ZENKOUHO, -1, -1, dp, k, uniq_level);
	    insert_dai(buf, ZENKOUHO, -1, -1, dp + k + 1, cnt - k - 1, uniq_level);
	}
	buf->c_zenkouho = 0;
    }else{
	insert_dai(buf, ZENKOUHO, -1, -1, dp, cnt, uniq_level);
	k = get_c_jikouho_from_zenkouho_dai(buf, buf->bun[bun_no]);
	if(k < 0){
	    k = 0;	/* Only when the kouho has been removed from dict. */
	}
	buf->c_zenkouho = k;
    }
    buf->zenkouho_bun = bun_no;
    buf->zenkouho_end_bun = bun_no2;
    buf->zenkouho_daip = DAI;
    for(k = 0 ; k < buf->zenkouho_suu; k++){
	if(buf->zenkouho[k]->ima && buf->zenkouho[k]->dic_no != -1){
	    add_down_bnst(buf, bun_no, buf->zenkouho[k]);
	}
    }
    return(buf->c_zenkouho);
}

int
jl_set_jikouho(buf, offset)
register struct wnn_buf *buf;
register int offset;
{
    wnn_errorno = 0;
    if(buf->zenkouho_suu <= 0)return(-1);
    if(buf->zenkouho_daip != SHO){
	return(-1);
    }
    offset = (offset + buf->zenkouho_suu) % buf->zenkouho_suu;
    if(buf->zenkouho_bun+1 < buf->bun_suu && buf->zenkouho_endvect != -1)
	buf->bun[buf->zenkouho_bun+1]->dai_top = buf->zenkouho[offset]->dai_end;
    free_sho(buf, &buf->bun[buf->zenkouho_bun]);
    set_sho(buf->zenkouho[offset], &buf->bun[buf->zenkouho_bun]);
    buf->c_zenkouho = offset;
    return(offset);
}


int
jl_set_jikouho_dai(buf, offset)
register struct wnn_buf *buf;
int offset;
{
    register int st, end, bun, k;

    wnn_errorno = 0;
    if(buf->zenkouho_suu <= 0)return(-1);
    if(buf->zenkouho_daip != DAI){
	return(-1);
    }
    offset = (offset + buf->zenkouho_dai_suu) % buf->zenkouho_dai_suu;
    if(buf->zenkouho_end_bun < buf->bun_suu && buf->zenkouho_endvect != -1)
	buf->bun[buf->zenkouho_end_bun]->dai_top =
		buf->zenkouho[buf->zenkouho_dai[offset+1]-1]->dai_end;
    free_bun(buf, buf->zenkouho_bun, buf->zenkouho_end_bun);
    st = buf->zenkouho_dai[offset];
    end = buf->zenkouho_dai[offset + 1];
    make_space_for(buf, BUN, buf->zenkouho_bun, buf->zenkouho_end_bun, end - st);
    for(bun = buf->zenkouho_bun, k = st; k < end;){
	set_sho(buf->zenkouho[k++], &buf->bun[bun++]);
    }
    buf->zenkouho_end_bun = buf->zenkouho_bun + end - st;
    buf->c_zenkouho = offset;
    return(offset);
}

int
jl_update_hindo(buf, bun_no, bun_no2)  
register struct wnn_buf *buf;
int bun_no, bun_no2;
{
    register int k;
    register WNN_BUN *wb;

    wnn_errorno = 0;
    if(bun_no < 0) return(-1);
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0) bun_no2 = buf->bun_suu;

#ifdef JL_DEBUG
    printf("\t\tDown Hindo\n");
#endif
    for(k = bun_no; k < bun_no2; k++){
	if(buf->bun[k]->hindo_updated == 1) continue;
	for(wb = buf->down_bnst[k]; wb; wb = wb->down){
	    if(wb->bug == 1) break;
	    wb->bug = 1;
	    if(wb->dic_no != -1){
		if(js_hindo_set(buf->env, wb->dic_no, wb->entry,
				WNN_IMA_OFF, WNN_HINDO_NOP) == -1) {
		    if (wnn_errorno == WNN_JSERVER_DEAD) {
			jl_disconnect(buf->env);buf->env=0;return(-1);
		    }
		}
#ifdef JL_DEBUG
    display_bun(wb);
#endif    	
	    }
	}
    }
    free_down(buf, bun_no, bun_no2);

#ifdef JL_DEBUG
    printf("\t\tUp Hindo\n");
#endif
    for(k = bun_no; k < bun_no2; k++){
	if(buf->bun[k]->hindo_updated == 1) continue;
	buf->bun[k]->hindo_updated = 1;
	wb = buf->bun[k];
#ifdef JL_DEBUG
	display_bun(wb);
#endif    	
	if(js_hindo_set(buf->env, wb->dic_no, wb->entry,
			 WNN_IMA_ON, WNN_HINDO_INC) == -1){
	    if (wnn_errorno == WNN_JSERVER_DEAD) {
		jl_disconnect(buf->env);buf->env=0;return(-1);
	    }
	}
    }
    return(0);
}


static w_char *
wnn_area(bp, area, kanjip)
WNN_BUN *bp;
w_char *area;
int kanjip;
{
    register WNN_BUN *bp1;
    register w_char *c, *end;

    for(bp1 = bp; bp1; bp1 = bp1->next){
	if(bp1 != bp) c = (w_char *)bp1;
	else c = bp1->yomi;
	end = (w_char *)&bp1->next;
	for(;c < end;){
	    if(!kanjip){
		if((*area++ = *c++) == 0){ area--; goto out;}
	    }else{
		if(*c++ == 0) kanjip--;
	    }
	}
    }
 out:
    return(area);
}	

static int
dai_end(buf, bun_no)
register struct wnn_buf *buf;
register int bun_no;
{
    bun_no++;
    for(;bun_no < buf->bun_suu && !buf->bun[bun_no]->dai_top; bun_no++);
    return(bun_no);
}

#define dai_end_zenkouho(buf, bun_no) (buf->zenkouho_dai[bun_no + 1])

#ifdef	CONVERT_by_STROKE
/* 筆形 (Bi Xing) */
void
jl_get_zenkouho_yomi(buf, zen_num, area)
register struct wnn_buf *buf;
int zen_num;
w_char *area;
{
    register int k, end;

    wnn_errorno = 0;
    if(!buf->zenkouho_daip){
	wnn_area(buf->zenkouho[zen_num], area, WNN_YOMI);
    }else{
	end = dai_end_zenkouho(buf, zen_num);
	for(k = buf->zenkouho_dai[zen_num]; k < end; k++){
	    area = wnn_area(buf->zenkouho[k], area, WNN_KANJI);
	}
    }
}
#endif

void
jl_get_zenkouho_kanji(buf, zen_num, area)
register struct wnn_buf *buf;
int zen_num;
w_char *area;
{
    register int k, end;

    wnn_errorno = 0;
    if(!buf->zenkouho_daip){
	wnn_area(buf->zenkouho[zen_num], area, WNN_KANJI);
    }else{
	end = dai_end_zenkouho(buf, zen_num);
	for(k = buf->zenkouho_dai[zen_num]; k < end; k++){
	    area = wnn_area(buf->zenkouho[k], area, WNN_KANJI);
	}
    }
}

int
wnn_get_area(buf, bun_no, bun_no2, area, kanjip)
struct wnn_buf *buf;
register int bun_no, bun_no2;
w_char *area;
int kanjip;
{
    register int k;
    w_char *area1 = area;

    if(bun_no < 0) return(0);
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0) bun_no2 = buf->bun_suu;

    for(k = bun_no; k < bun_no2; k++){
	area = wnn_area(buf->bun[k], area, kanjip);
    }
    return(area - area1);
}

#define JISHO 1
#define HINDO 2

/*********************************/
int
jl_dic_add_e(env,dic_name,hindo_name,rev, prio,rw, hrw, pwd_dic, pwd_hindo, error_handler, message_handler)
register struct wnn_env *env;
char *dic_name;
char *hindo_name;
int prio;
int rw, hrw, rev;
char *pwd_dic, *pwd_hindo;
int  (*error_handler)(), (*message_handler)();
{
    char tmp[256];
    char pwd[WNN_PASSWD_LEN], hpwd[WNN_PASSWD_LEN];
    int fid, hfid = -1;
    register int ret;


    if(file_exist(env, dic_name) == -1) {
	if (wnn_errorno == WNN_JSERVER_DEAD) {
	    jl_disconnect(env);return(-1);
	}
	if((int)error_handler == WNN_NO_CREATE || 
	   (rw == WNN_DIC_RDONLY)){
	    sprintf(tmp, "%s \"%s\" %s",
		    msg_get(wnn_msg_cat, 200, NULL, env->lang),
		    dic_name,
		    msg_get(wnn_msg_cat, 201, NULL, env->lang));
		    /*
		    "辞書ファイル \"%s\" が無いよ。",
		    */
	    message_out(message_handler, tmp);
	    wnn_errorno = WNN_NO_EXIST;
	    return(-1);
	}
	sprintf(tmp, "%s \"%s\" %s%s",
		msg_get(wnn_msg_cat, 200, NULL, env->lang),
		dic_name,
		msg_get(wnn_msg_cat, 201, NULL, env->lang),
		msg_get(wnn_msg_cat, 202, NULL, env->lang));
		/*
		"辞書ファイル \"%s\" が無いよ。作る?(Y/N)",
		*/
	if((int)error_handler == WNN_CREATE || 
	   call_error_handler(error_handler, tmp)){
	    if(create_file(env, dic_name,JISHO, -1, /* -1 is dummy */
			   pwd_dic, 
			   (hindo_name && *hindo_name)? "": pwd_hindo,
			   error_handler, message_handler) == -1) {
		return(-1);
	    }
	}else{
	    wnn_errorno = WNN_NO_EXIST;
	    return(-1);
	}
    }
    if((fid = file_read(env, dic_name)) == -1)
	    if_dead_disconnect(env, -1);
    if(hindo_name && *hindo_name){
	if(file_exist(env, hindo_name) == -1) {
	    if(wnn_errorno == WNN_JSERVER_DEAD)	{
		jl_disconnect(env);return(-1);
	    }
	    if((int)error_handler == WNN_NO_CREATE ||
	       (hrw == WNN_DIC_RDONLY)){
		sprintf(tmp, "%s \"%s\" %s",
			msg_get(wnn_msg_cat, 203, NULL, env->lang),
			hindo_name,
			msg_get(wnn_msg_cat, 201, NULL, env->lang));
			/*
			"頻度ファイル \"%s\" が無いよ。",
			*/
		message_out(message_handler, tmp);
		wnn_errorno = WNN_NO_EXIST;
		return(-1);
	    }
	    sprintf(tmp, "%s \"%s\" %s%s",
		    msg_get(wnn_msg_cat, 203, NULL, env->lang),
		    hindo_name,
		    msg_get(wnn_msg_cat, 201, NULL, env->lang),
		    msg_get(wnn_msg_cat, 202, NULL, env->lang));
		    /*
		    "頻度ファイル \"%s\" が無いよ。作る?(Y/N)",
		    */
	    if((int)error_handler == WNN_CREATE || 
	       call_error_handler(error_handler, tmp)){
		if(create_file(env, hindo_name, HINDO, fid, 
			       "", pwd_hindo, error_handler,
			       message_handler) == -1) return(-1);
	    }else{
		wnn_errorno = WNN_NO_EXIST;
		return(-1);
	    }
	}
	if((hfid = file_read(env, hindo_name)) == -1){
	    if_dead_disconnect(env, -1);
	}
    }
    if(get_pwd(pwd_dic, pwd) == -1)return(-1);
    if(get_pwd(pwd_hindo, hpwd) == -1)return(-1);
    if((ret = js_dic_add(env, fid, hfid, rev, prio, rw, hrw, pwd, hpwd)) < 0){
	if(wnn_errorno == WNN_JSERVER_DEAD){
	    jl_disconnect(env);
	    return(-1);
	}else if(wnn_errorno == WNN_HINDO_NO_MATCH){
	    if((int)error_handler == WNN_NO_CREATE){
		return(-1);
	    }
	    sprintf(tmp,
		    msg_get(wnn_msg_cat, 204, NULL, env->lang),
		    hindo_name);
		    /*
		    "辞書と頻度 \"%s\" の整合性が無いよ。作り直す?(Y/N)",
		    */
	    if(!((int)error_handler == WNN_CREATE ||
		 call_error_handler(error_handler, tmp))){
		return(-1);
	    }
	    if(file_discard(env, hfid) == -1)
		if_dead_disconnect(env, -1);
	    if(file_remove(env->js_id, hindo_name, hpwd) == -1)
		if_dead_disconnect(env, -1);
	    if(create_file(env,hindo_name, HINDO, fid, NULL, pwd_hindo, WNN_CREATE, message_handler) == -1) return(-1);
	    if((hfid = file_read(env, hindo_name)) == -1)
		if_dead_disconnect(env, -1);
	    if((ret = js_dic_add(env, fid, hfid, rev, prio, rw, hrw, pwd, hpwd))< 0)
		if_dead_disconnect(env, -1);
	}
    }
    return(ret);
}


int
jl_dic_delete_e(env, dic_no)
register struct wnn_env *env;
register int dic_no;
{
    WNN_DIC_INFO dic;

    if(js_dic_info(env,dic_no,&dic) < 0){
	if_dead_disconnect(env, -1);
    }
    if(js_dic_delete(env, dic_no) < 0)
	if_dead_disconnect(env, -1);
    /*	dic Body	*/
    if (file_discard(env,dic.body) < 0) {
	if_dead_disconnect(env, -1);
    }
    /*	dic hindo	*/
    if(dic.hindo != -1){
	if (file_discard(env,dic.hindo) < 0){
	    if_dead_disconnect(env, -1);
	}
    }
    return(0);
}

static int
get_pwd(pwd_dic, pwd)
register char *pwd_dic, *pwd;
{
    FILE *fp;
	    
    if(pwd_dic && *pwd_dic){
	if((fp = fopen(pwd_dic, "r")) == NULL){
	    wnn_errorno = WNN_CANT_OPEN_PASSWD_FILE;
	    return(-1);
	}
	fgets(pwd, WNN_PASSWD_LEN, fp);
	fclose(fp);
    }else {
	pwd[0] = 0;
    }
    return(0);
}

static int
create_pwd_file(env, pwd_file, error_handler, message_handler)
register struct wnn_env *env;
char *pwd_file;
int  (*error_handler)(), (*message_handler)();
{
    FILE *fp;
    char gomi[256];

    if(pwd_file == NULL || *pwd_file == 0) return(0);
    if(access(pwd_file, F_OK) != -1) return(0);
    sprintf(gomi, "%s \"%s\" %s%s",
	    msg_get(wnn_msg_cat, 205, NULL, env->lang),
	    pwd_file,
	    msg_get(wnn_msg_cat, 201, NULL, env->lang),
	    msg_get(wnn_msg_cat, 202, NULL, env->lang));
	    /*
	    "password_file \"%s\" が無いよ。作る?(Y/N)",
	    */
    if(call_error_handler(error_handler,gomi) == 0){
	wnn_errorno = WNN_NO_EXIST;
	return(-1);
    }
    if((fp = fopen(pwd_file, "w")) == NULL){
	wnn_errorno = WNN_CANT_OPEN_PASSWD_FILE;
	message_out(message_handler, wnn_perror_lang(env->lang));
	return(-1);
    }
#ifdef HITACHI
    srand(time(0)+getuid());
    fprintf(fp,"%d\n",rand());
#else /* HITACHI */
#ifdef SYSVR2
    srand(time(0)+getuid());
    fprintf(fp,"%d\n",rand());
#else
    srandom(time(0)+getuid());
    fprintf(fp,"%d\n",random());
#endif
#endif /* HITACHI */
    fclose(fp);
#define	MODE_PWD (0000000 | 0000400)
    chmod(pwd_file,MODE_PWD);
    return(0);
}


/**	jl_fuzokugo_set	**/
int
jl_fuzokugo_set_e(env,fname)
struct wnn_env *env;
char *fname;
{
    register int fid, orgfid;
    int	ret;

    wnn_errorno = 0;
    orgfid = js_fuzokugo_get(env);
    /* If orgfid == -1, it must be
       because no fuzokugo_file is set to the env
       */
    if((fid=file_read(env,fname)) == -1)
	if_dead_disconnect(env, -1);
    if ((ret = js_fuzokugo_set(env,fid)) < 0)
	if_dead_disconnect(env, ret);
    if(fid != orgfid && orgfid != -1){
	js_file_discard(env, orgfid);
    }
    return(ret);
}

/**	jl_fuzokugo_get	**/
int
jl_fuzokugo_get_e(env, fname)
register struct wnn_env *env;
char *fname;
{
    WNN_FILE_INFO_STRUCT file;
    int fid;
    char *c;

    wnn_errorno = 0;
    fname[0] = 0;
    if((fid = js_fuzokugo_get(env)) < 0)
	if_dead_disconnect(env, -1);
    if (js_file_info(env,fid,&file) < 0)
	if_dead_disconnect(env, -1);
    c = find_file_name_from_id(env, fid);
    if(c == NULL){
	c = file.name;
    }
    strcpy(fname, c);
    return(fid);
}

/**	jl_dic_save	**/
int
jl_dic_save_e(env,dic_no)
register struct wnn_env *env;
int	dic_no;
{
    WNN_DIC_INFO dic;
    WNN_FILE_INFO_STRUCT file;
    char *c;

    wnn_errorno = 0;
    if(js_dic_info(env,dic_no,&dic) < 0)
	    if_dead_disconnect(env, -1);
    /*	dic Body	*/
    c = find_file_name_from_id(env, dic.body);
    if(c == NULL){
	if(dic.localf){
	    c = dic.fname;
	}else{
	    wnn_errorno=WNN_FILE_NOT_READ_FROM_CLIENT;
	    return(-1); 
	}
    } 
    if(c[0] != C_LOCAL){
	if (js_file_write(env,dic.body,c) < 0) {
	    if (wnn_errorno == WNN_JSERVER_DEAD) {
		jl_disconnect_if_server_dead(env);
		return(-1);
	    }
	}
    }else{
	if (js_file_receive(env,dic.body,c + 1) < 0) {
	    if (wnn_errorno == WNN_JSERVER_DEAD) {
		jl_disconnect_if_server_dead(env);
		return(-1);
	    }
	}
    }
 /*	dic hindo	*/
    if(dic.hindo != -1){
	if (js_file_info(env,dic.hindo,&file) < 0)
	    if_dead_disconnect(env, -1);
	c = find_file_name_from_id(env, file.fid);
	if(c == NULL){
	    if(dic.hlocalf){
		c = dic.hfname;
	    }else{
		wnn_errorno=WNN_FILE_NOT_READ_FROM_CLIENT;
		return(-1);
	    }
	}
	if(c[0] != C_LOCAL){
	    if (js_file_write(env,dic.hindo,c) < 0) {
		if_dead_disconnect(env, -1);
	    }
	}else{
	    if (js_file_receive(env,dic.hindo,c + 1) < 0) {
		if_dead_disconnect(env, -1);
	    }
	}
    }
    return(0);
}

int
jl_dic_save_all_e(env)
struct wnn_env *env;
{
    register WNN_DIC_INFO *dic;
    register int k;
    char *c;

    register int cnt;

    wnn_errorno = 0;
    if((cnt = js_dic_list(env, &dicrb)) == -1)
	    if_dead_disconnect(env, -1);
    dic = (WNN_DIC_INFO *)dicrb.buf;
    for(k = 0 ; k < cnt ; k++, dic++){
	if((c = find_file_name_from_id(env, dic->body)) == NULL){
	    if(dic->localf){
		c = dic->fname;
	    }else{
		wnn_errorno=WNN_FILE_NOT_READ_FROM_CLIENT;
	    }
	}
	if(c){
	    if(c[0] != C_LOCAL){
		if (js_file_write(env,dic->body,c) < 0) {
		    if (wnn_errorno == WNN_JSERVER_DEAD) {
			jl_disconnect_if_server_dead(env);
			return(-1);
		    }
		}
	    }else{
		if (js_file_receive(env,dic->body,c + 1) < 0) {
		    if (wnn_errorno == WNN_JSERVER_DEAD) {
			jl_disconnect_if_server_dead(env);
			return(-1);
		    }
		}
	    }
	}
 /*	dic hindo	*/
	if(dic->hindo != -1){
	    c = find_file_name_from_id(env, dic->hindo);
	    if(c == NULL){
		if(dic->hlocalf){
		    c = dic->hfname;
		}else{
		    wnn_errorno=WNN_FILE_NOT_READ_FROM_CLIENT;
		}
	    }
	    if(c){
		if(c[0] != C_LOCAL){
		    if (js_file_write(env,dic->hindo,c) < 0) {
			if (wnn_errorno == WNN_JSERVER_DEAD) {
			    if_dead_disconnect(env, -1);
			}
		    }
		}else{
		    if (js_file_receive(env,dic->hindo,c + 1) < 0) {
			if (wnn_errorno == WNN_JSERVER_DEAD) {
			    if_dead_disconnect(env, -1);
			}
		    }
		}
	    }
	}
    }
    if(wnn_errorno) return(-1);
    return(0);
}

/*
 *
 * bun manipulate  routines
 *
 */

static void
free_sho(buf, wbp)
register struct wnn_buf *buf;
WNN_BUN **wbp;
{
    register WNN_BUN *wb;
    wb = *wbp;

    if(--wb->ref_cnt <= 0){
	for(; wb;){
	    wb->free_next = buf->free_heap;
	    buf->free_heap = wb;
	    wb = wb->next;
	}
    }
    *wbp = NULL;
}

static void
free_zenkouho(buf)
register struct wnn_buf *buf;
{
    register int k;

    for(k = 0 ; k < buf->zenkouho_suu; k++){
	free_sho(buf, &buf->zenkouho[k]);
    }
    buf->zenkouho_suu = 0;
    buf->zenkouho_dai_suu = 0;
    buf->c_zenkouho = -1;
    buf->zenkouho_bun = -1;
    buf->zenkouho_end_bun = -1;
}

static void
free_bun(buf, bun_no, bun_no2)
struct wnn_buf *buf;
register int bun_no, bun_no2;
{
    register int k;

    for(k = bun_no; k < bun_no2; k++){
	free_sho(buf, &buf->bun[k]);
    }
}

static void
free_down(buf, bun_no, bun_no2)
struct wnn_buf *buf;
int bun_no, bun_no2;
{
    register WNN_BUN **wbp, **wbp1;
    int k;

    for(k = bun_no; k < bun_no2; k++){
	for(wbp = &buf->down_bnst[k]; *wbp; wbp = wbp1){
	    wbp1 = &(*wbp)->down;
	    free_sho(buf, wbp);
	}
    }
}

static
WNN_BUN *
get_new_bun(buf)
register struct wnn_buf *buf;
{
    register WNN_BUN *wb;


    if(buf->free_heap == NULL){
	if(alloc_heap(buf, INCREMENT) == -1)return(NULL);
    }
    wb = buf->free_heap;
    buf->free_heap = wb->free_next;
    wb->free_next = NULL;
    wb->daihyoka = -1;

    return(wb);
}

static
WNN_BUN *
get_sho(buf, sb, zenp, daip)
struct wnn_buf *buf;
struct wnn_sho_bunsetsu *sb;
int zenp, daip;
{
    register w_char *c, *end, *s;
    register WNN_BUN *wb;
    WNN_BUN *wb1;
    int where = 1;
    int len;

    if((wb = get_new_bun(buf)) == NULL) return(NULL);

    wb->jirilen = sb->jiriend - sb->start + 1;
    wb->dic_no = sb->dic_no;
    wb->entry = sb->entry;
    wb->kangovect = sb->kangovect;
    wb->hinsi = sb->hinsi;	
    wb->hindo = sb->hindo;	
    wb->ima = sb->ima;
    wb->hindo_updated = 0;
    wb->bug = 0;
    wb->dai_top = 0;
    wb->nobi_top = 0;
    wb->ref_cnt = 1;		
    wb->hyoka = sb->hyoka;
    wb->down = NULL;
    wb->from_zenkouho = daip << 1 | zenp;
    len = wnn_Strlen(sb->fuzoku);
    wb->yomilen = wnn_Strlen(sb->yomi) + len;
    wb->kanjilen = wnn_Strlen(sb->kanji) + len;
/*
    wb->dai_top = (sb->status == WNN_CONNECT)? 0:1;
大文節の先頭以外の小文節に関しては、status はいい加減な値が入っている。
*/  
    s = sb->yomi;

    for(wb1 = wb; ;){
	if(wb1 == wb) c = wb1->yomi;
	else c = (w_char *)wb1;
	end = (w_char *)&wb1->next;

	for(; c < end;) {
	    if((*c++ = *s++) == 0){
		if(where == 1){
		    where = 3;
		    c--;
		    s = sb->fuzoku;
		}else if(where == 3){
		    where = 0;
		    s = sb->kanji;
		}else if(where == 0){
		    where = 4;
		    c--;
		    s = sb->fuzoku;
		}else{
		    goto out;
		}
	    }
	}
	wb1->next = get_new_bun(buf);
	wb1 = wb1->next;
    }
    out:
    wb1->next = NULL;
    return(wb);
}

static void
make_space_for(buf, zenp, bun_no, bun_no2, cnt)
register struct wnn_buf *buf;
int bun_no, bun_no2, zenp;
int cnt;
{
    switch(zenp){
    case BUN:
	make_space_for_bun(buf,bun_no, bun_no2, cnt);
	break;
    case ZENKOUHO:
	make_space_for_zenkouho(buf,bun_no, bun_no2, cnt);
    }
}

static void
make_space_for_bun(buf, bun_no, bun_no2, cnt)
register struct wnn_buf *buf;
int bun_no, bun_no2;
int cnt;
{
    int newsize;
    register int k;

    newsize = buf->bun_suu + cnt - (bun_no2 - bun_no);

#define Realloc(a, b) realloc((char *)(a), (unsigned)(b))

    if(newsize > buf->msize_bun){
	buf->bun = (WNN_BUN **)Realloc(buf->bun, newsize * sizeof(WNN_BUN *));
	buf->down_bnst = (WNN_BUN **)Realloc(buf->down_bnst, newsize * sizeof(WNN_BUN *));
	buf->msize_bun = newsize;
    }


    for(k = buf->bun_suu; k < newsize; k++){
	buf->down_bnst[k] = NULL;
    }
    bcopy((char *)&buf->bun[bun_no2], (char *)&buf->bun[bun_no + cnt],
	  (buf->bun_suu - bun_no2) * sizeof(WNN_BUN *));
    bcopy((char *)&buf->down_bnst[bun_no2], (char *)&buf->down_bnst[bun_no + cnt],
	  (buf->bun_suu - bun_no2) * sizeof(WNN_BUN *));
    if(bun_no2 < bun_no + cnt){
	bzero((char *)&buf->down_bnst[bun_no2], (bun_no + cnt - bun_no2) * sizeof(WNN_BUN *));
    }
    buf->bun_suu = newsize;
}

static void
make_space_for_zenkouho(buf, bun_no, bun_no2, cnt)
struct wnn_buf *buf;
int bun_no, bun_no2;
register int cnt;
{
    register int newsize;

    newsize = buf->zenkouho_suu + cnt - (bun_no2 - bun_no);

    if(newsize > buf->msize_zenkouho){
	buf->zenkouho = (WNN_BUN **)Realloc(buf->zenkouho, newsize * sizeof(WNN_BUN *));
	buf->zenkouho_dai = (int *)Realloc(buf->zenkouho_dai, (1 + newsize) * sizeof(int *));
	buf->msize_zenkouho = newsize;
    }
    bcopy((char *)&buf->zenkouho[bun_no2],
	  (char *)&buf->zenkouho[bun_no + cnt],
	  (buf->zenkouho_suu - bun_no2) * sizeof(WNN_BUN *));
    buf->zenkouho_suu = newsize;
}


static int
insert_sho(buf, zenp, bun_no, bun_no2, sp, cnt, uniq_level)
struct wnn_buf *buf;
int bun_no, bun_no2;
register struct wnn_sho_bunsetsu *sp;
int cnt;
int zenp;			/* daip */
int uniq_level;			/* uniq is only supported when bun_no = -1
				   and zenp == ZENKOUHO */
{
    register WNN_BUN **b;
    register int k;

    if(bun_no == -1){
	bun_no = bun_no2 = (zenp == BUN)? buf->bun_suu: buf->zenkouho_suu;
    }

    /* It will make too big space when uniq_level > 0, but That's OK! */
    make_space_for(buf, zenp, bun_no, bun_no2, cnt);

    b = ((zenp == BUN)? buf->bun: buf->zenkouho) + bun_no;
    for(k = bun_no ; k < bun_no + cnt ; k++, sp++){
	if(uniq_level){
	    if(find_same_kouho(sp, buf->zenkouho, b,uniq_level))continue;
	}
	*b = get_sho(buf, sp, zenp, SHO);
	(*b)->dai_top = (sp->status == WNN_CONNECT)? 0:1;
	if(zenp != BUN){
	    if(buf->zenkouho_endvect != -1){
		(*b)->dai_end = (sp->status_bkwd == WNN_CONNECT_BK)? 0:1;
	    }else{
		(*b)->dai_end = 1;
	    }
	}
	b++;
    }
    if(uniq_level && zenp == ZENKOUHO){
	buf->zenkouho_suu = b - buf->zenkouho;
    }
    return(cnt + bun_no);
}

	/* for zenkouho, assume bun_no = bun_no2 = zenkouho_suu */
static int
insert_dai(buf, zenp, bun_no, bun_no2, dp, dcnt, uniq_level)
struct wnn_buf *buf;
int bun_no, bun_no2;
struct wnn_dai_bunsetsu *dp;
int dcnt;
int zenp;
int uniq_level;
{
    register WNN_BUN **b, **b0;
    register int k, l, m;
    register int cnt = 0;
    struct wnn_sho_bunsetsu *sp, *sp1;

    if(bun_no == -1){
	bun_no = bun_no2 = (zenp == BUN)? buf->bun_suu: buf->zenkouho_suu;
    }

    for(k = 0; k < dcnt ; k++){
	cnt += dp[k].sbncnt;
    }
    make_space_for(buf, zenp, bun_no, bun_no2, cnt);
				/* zenkouho_dai_suu must not be initialized */

    b = ((zenp == BUN)? buf->bun: buf->zenkouho) + bun_no;

    for(k = 0, m = buf->zenkouho_dai_suu ; k < dcnt; k++){
	if(uniq_level){
	    if(find_same_kouho_dai(&dp[k], buf, m, uniq_level))
		continue;
	}
	sp = dp[k].sbn;
	if(zenp == ZENKOUHO){
	    buf->zenkouho_dai[m++] = b - buf->zenkouho;
	}
	b0 = b;
	sp1 = sp;
	for(l = 0 ; l < dp[k].sbncnt; l++){
	    *b = get_sho(buf, sp, zenp, DAI);
	    if(zenp == ZENKOUHO){
		if (l == dp[k].sbncnt -1){
		    if(buf->zenkouho_endvect != -1){
			(*b)->dai_end = (sp->status_bkwd == WNN_CONNECT_BK)? 0:1;
		    }else{
			(*b)->dai_end = 0;
		    }
		}else{
		    (*b)->dai_end = 0;
		}
	    }
	    *b++;
	    sp++;
	}
	(*b0)->dai_top = (sp1->status == WNN_CONNECT)? 0:1;
	(*b0)->daihyoka = dp[k].hyoka;
    }
    if(zenp == ZENKOUHO){
	buf->zenkouho_dai[m] = b - buf->zenkouho;
	buf->zenkouho_suu = b - buf->zenkouho;
	buf->zenkouho_dai_suu = m;
    }
    return(cnt + bun_no);
}

static void
set_sho(b, p)
register WNN_BUN *b;
register WNN_BUN **p;
{
    b->ref_cnt++;
    *p = b;
}

static void
set_dai(b, p, n)
register WNN_BUN **b;
register WNN_BUN **p;
register int n;
{
    for(;n;n--){
	(*b)->ref_cnt++;
	*p++ = *b++;
    }
}

static int
get_c_jikouho_from_zenkouho(buf, dest)
struct wnn_buf *buf;
WNN_BUN *dest;
{
    register int k;
    w_char area[LENGTHKANJI];
    w_char area1[LENGTHKANJI];
    register WNN_BUN *b;

    wnn_area(dest, area, WNN_KANJI);
    for(k = 0; k < buf->zenkouho_suu; k++){
	b = buf->zenkouho[k];
	if(b->entry == dest->entry && b->dic_no == dest->dic_no){
	    wnn_area(b, area1, WNN_KANJI);
	    if(wnn_Strcmp(area, area1) == 0){
		return(k);
	    }
	}
    }
    return(-1);
}

static int
get_c_jikouho_from_zenkouho_dai(buf, dest)
struct wnn_buf *buf;
WNN_BUN *dest;
{
    register int k;
    w_char area[LENGTHKANJI];
    w_char area1[LENGTHKANJI];
    register WNN_BUN *b;
    register int l;

    wnn_area(dest, area, WNN_KANJI);
    for(k = 0; k < buf->zenkouho_dai_suu; k++){
	b = buf->zenkouho[buf->zenkouho_dai[k]];
	for(l = 0 ; l < buf->zenkouho_dai[k + 1]; l++, dest++, b++){
	    if(b->entry != dest->entry || b->dic_no != dest->dic_no) break;
	    wnn_area(b, area1, WNN_KANJI);
	    if(wnn_Strcmp(area, area1) != 0){
		break;
	    }
	}
	if(l == buf->zenkouho_dai[k + 1]){
	    return(k);
	}
    }
    return(-1);
}


static int
get_c_jikouho(sp, cnt, dest)
struct wnn_sho_bunsetsu *sp;
int cnt;
WNN_BUN *dest;
{
    register int k;
    register int len;
    w_char area[LENGTHKANJI];

    wnn_area(dest, area, WNN_KANJI);
    for(k = 0; k < cnt; k++,sp++){
	if(sp->entry == dest->entry && sp->dic_no == dest->dic_no
	   && sp->kangovect == dest->kangovect){
	    if(wnn_Strncmp(area, sp->kanji, len = wnn_Strlen(sp->kanji)) == 0 &&
	       wnn_Strcmp(area + len, sp->fuzoku) == 0){
		return(k);
	    }
	}
    }
    return(-1);
}

static int
get_c_jikouho_dai(dp, cnt, dest, bun_no)
struct wnn_dai_bunsetsu *dp;
int cnt;
WNN_BUN **dest;
int bun_no;
{
    register int k, l;
    register int len;
    w_char area[LENGTHKANJI];
    register struct wnn_sho_bunsetsu *sp;    

    for(k = 0; k < cnt; k++,dp++){
	sp = dp->sbn;
	for(l = 0 ; l < dp->sbncnt; l++, sp++){
	    if(sp->entry != (dest[bun_no + l])->entry ||
	       sp->kangovect != (dest[bun_no + l])->kangovect ||
	       sp->dic_no != (dest[bun_no + l])->dic_no){
		break;
	    }
	    wnn_area(dest[bun_no + l], area, WNN_KANJI);
	    if(wnn_Strncmp(area, sp->kanji, len = wnn_Strlen(sp->kanji)) != 0 ||
	       wnn_Strcmp(area + len, sp->fuzoku) != 0){
		break;
	    }
	}
	if(l == dp->sbncnt) return(k);
    }
    return(-1);
}


static int
find_same_kouho(sp, st, end, level)
struct wnn_sho_bunsetsu *sp;
register WNN_BUN **st, **end;
int level;
{
    register int len;
    w_char area[LENGTHKANJI];
    register WNN_BUN *b;

    if(level == WNN_UNIQ){
	for(; st < end; st++){
	    b = *st;
	    if(sp->hinsi == b->hinsi){
		wnn_area(b, area, WNN_KANJI);
		if(wnn_Strncmp(area, sp->kanji, len = wnn_Strlen(sp->kanji)) == 0 &&
		   wnn_Strcmp(area + len, sp->fuzoku) == 0){
		    return(1);
		}
	    }
	}
    }else{ /* level = WNN_UNIQ_KNJ */
	for(; st < end; st++){
	    b = *st;
	    wnn_area(b, area, WNN_KANJI);
	    if(wnn_Strncmp(area, sp->kanji, len = wnn_Strlen(sp->kanji)) == 0 &&
	       wnn_Strcmp(area + len, sp->fuzoku) == 0){
		return(1);
	    }
	}
    }	
    return(0);
}
    
static int
find_same_kouho_dai(dp, buf, top, level)
struct wnn_dai_bunsetsu *dp;
struct wnn_buf *buf;
int top;
int level;
{
    int len;
    register int k, l;
    w_char area[LENGTHKANJI];
    WNN_BUN *b;
    register struct wnn_sho_bunsetsu *sp;

    for(k = 0 ; k < top ; k++){
	for(l = 0,sp = dp->sbn ; l < dp->sbncnt; l++, sp++){
	    b = buf->zenkouho[buf->zenkouho_dai[k] + l];
	    if(sp->end - sp->start + 1 != b->yomilen) break;/* From: tsuiki */
	    if(level != WNN_UNIQ_KNJ){
		if(sp->hinsi != b->hinsi) break;
	    }
	    wnn_area(b, area, WNN_KANJI);
	    if(wnn_Strncmp(area, sp->kanji, len = wnn_Strlen(sp->kanji)) != 0 ||
	       wnn_Strcmp(area + len, sp->fuzoku) != 0){
		break;
	    }
	}
	if(l == dp->sbncnt)
	    return(1);
    }
    return(0);
}

int
wnn_cnt_free(buf)
struct wnn_buf *buf;
{
    register int n;
    register WNN_BUN *b;

    for(n = 0, b = buf->free_heap ;b;n++, b = b->free_next);
    return(n);
}


struct wnn_jdata *
jl_word_info_e(env, dic_no, entry)
register struct wnn_env *env;
int dic_no, entry;
{
    wnn_errorno = 0;
    if (js_word_info(env,dic_no, entry, &wordrb) < 0)
	if_dead_disconnect(env, NULL);
    return((struct wnn_jdata *)(wordrb.buf));
}    

int
jl_dic_list_e(env, dicinfo)
struct wnn_env *env;
WNN_DIC_INFO **dicinfo;
{
    WNN_DIC_INFO *info;
    int cnt;
    register int k;
    register char *c;

    wnn_errorno = 0;
    if ((cnt = js_dic_list(env, &dicrb)) < 0)
	if_dead_disconnect(env, -1);
    info = (WNN_DIC_INFO *)(dicrb.buf);

/* If the file is loaded from this client, change the file name to the one
   used in loading it. */
    for(k = 0 ; k < cnt ; k++){
	c = find_file_name_from_id(env, info[k].body);
	if(c != NULL){
	    strcpy(info[k].fname, c);
	}

	c = find_file_name_from_id(env, info[k].hindo);
	if(c != NULL){
	    strcpy(info[k].hfname, c);
	}
    }
    *dicinfo = info;
    return(cnt);
}


static int
sort_func_ws(a,b)
register char *a, *b;
{
    int ah, bh, ai, bi, iah, ibh, iai, ibi;
    ah = ((struct wnn_jdata *)a)->hindo;
    bh = ((struct wnn_jdata *)b)->hindo;
    iah = ((struct wnn_jdata *)a)->int_hindo;
    ibh = ((struct wnn_jdata *)b)->int_hindo;
    ai = ((struct wnn_jdata *)a)->ima;
    bi = ((struct wnn_jdata *)b)->ima;
    iai = ((struct wnn_jdata *)a)->int_ima;
    ibi = ((struct wnn_jdata *)b)->int_ima;

    if(ai == WNN_IMA_OFF && ah == WNN_ENTRY_NO_USE) return(1);
    if(bi == WNN_IMA_OFF && bh == WNN_ENTRY_NO_USE) return(-1);
    if(iai == WNN_IMA_OFF && iah == WNN_ENTRY_NO_USE) return(1);
    if(ibi == WNN_IMA_OFF && ibh == WNN_ENTRY_NO_USE) return(-1);

    if(ai != bi){
	if(ai < bi) return(1);
	return(-1);
    }
    if(iah >= 0){
	ah += iah;
	bh += ibh;
    }
    if(ah > bh)return(-1);
    if(ah < bh)return(1);
    return(0);
}

int
jl_word_search_e(env,dic_no, yomi, jdp)
register struct wnn_env *env;
int dic_no;
w_char *yomi;
struct wnn_jdata **jdp;
{
    register int cnt;
    struct wnn_jdata *jd;

    wnn_errorno = 0;
    if ((cnt = js_word_search(env,dic_no, yomi, &wordrb)) < 0)
	if_dead_disconnect(env, -1);
    jd = (struct wnn_jdata *)wordrb.buf;
/*    for(cnt = 0 ; jd[cnt].dic_no != -1; cnt++); */
    qsort((char *)jd,cnt,sizeof(struct wnn_jdata),sort_func_ws);
    *jdp = jd;
    return(cnt);
}

int 
jl_word_search_by_env_e(env, yomi, jdp)
register struct wnn_env *env;
struct wnn_jdata **jdp;
w_char *yomi;
{
    register int cnt;
    struct wnn_jdata *jd;

    wnn_errorno = 0;
    if ((cnt = js_word_search_by_env(env, yomi, &wordrb)) < 0)
	if_dead_disconnect(env, -1);
    jd = (struct wnn_jdata *)wordrb.buf;
/*    for(cnt = 0 ; jd[cnt].dic_no != -1; cnt++); */
    qsort((char *)jd,cnt,sizeof(struct wnn_jdata),sort_func_ws);
    *jdp = jd;
    return(cnt);
}

#ifdef JL_DEBUG
static void
display_bun(b)
WNN_BUN *b;
{
    w_char yomi[LENGTHBUNSETSU];
    wnn_area(b, yomi, 1); putws(yomi);
    printf("\t");
    wnn_area(b, yomi, 0); putws(yomi);
    printf("\t");
    printf("Jirilen:%d Dic_no:%d Serial:%d Hinsi:%d Hindo:%c%d\n Hindo_Updated:%d Nobi_Top:%d Dai_top:%d Ref_cnt:%d Down:%d\n",
	   b->jirilen, b->dic_no, b->entry, b->hinsi, 
	   (b->ima )? '*':' ',
	   b->hindo, b->hindo_updated, b->nobi_top, b->dai_top, b->ref_cnt, b->down);
}
#endif


static void
add_down_bnst(buf, k, b)
register struct wnn_buf *buf;
register int k;
register WNN_BUN *b;
{
    if(b->down) return; /* In order to prevent roop! */
    if(b == buf->down_bnst[k]) return; /* In order to prevent roop! */
				/* It occurs when Zenkouho-->Nobi-conv */
    b->down = buf->down_bnst[k];
    buf->down_bnst[k] = b;
    b->ref_cnt ++;
}

#ifdef JL_DEBUG
static void
print_jdata(jd)
struct wnn_jdata *jd;
{
    putws(jd->kanji);
    printf("\tDict:%d Serial:%d Hinsi:%s Hindo:%c%d ExHindo:%c%d\n",
	   jd->dic_no, jd->serial, wnn_get_hinsi_name(jd->hinshi),
	   (jd->ima)? '*':' ', jd->hindo,
	   (jd->int_ima)? '*':' ', jd->int_hindo);
}
#endif

#define REAL_PARAM(x) (strcmp(x, "-"))


/** wnnrc を見てのパラメータの設定 */
int
jl_set_env_wnnrc(env, wnnrc_n, error_handler, message_handler)
register struct wnn_env *env;
char *wnnrc_n;
int  (*error_handler)(), (*message_handler)();
{
    int level = 0;
    int x;
    wnn_errorno = 0;
    if((int)error_handler ==  WNN_CREATE){
	confirm_state = CREATE_WITHOUT_CONFIRM;
    }else if((int)error_handler ==  WNN_NO_CREATE){
	confirm_state = NO_CREATE;
    }else{
	confirm_state = CONFIRM;
    }
    x = jl_set_env_wnnrc1(env, wnnrc_n, error_handler, message_handler, level);
    confirm_state = 0;
    return(x);
}	

int
jl_set_env_wnnrc1(env, wnnrc_n, error_handler, message_handler, level)
register struct wnn_env *env;
char *wnnrc_n;
int  (*error_handler)(), (*message_handler)();
int level;
{
    register int num;
    char s[20][EXPAND_PATH_LENGTH];
    char code[EXPAND_PATH_LENGTH];
    char tmp[1024];
    register FILE *fp;

    wnn_errorno = 0;
    if(level > MAXINCLUDE){
	message_out(message_handler,
		    msg_get(wnn_msg_cat, 206, NULL, env->lang));
		    /*
		    "include のレベルが多過ぎます。"
		    */
	return(-1);
    }
    if((fp = fopen(wnnrc_n, "r")) == NULL){
	message_out(message_handler,
		    msg_get(wnn_msg_cat, 207, NULL, env->lang), wnnrc_n);
		    /*
		    "file \"%s\"が open できません",
		    */
	return(-1);
    }
    while(fgets(tmp,1024,fp  ) != NULL){
	num = sscanf(tmp,
	"%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s",code,
		     s[0],s[1],s[2],s[3],s[4],s[5],s[6],s[7],s[8],s[9],s[10],
		     s[11],s[12],s[13],s[14],s[15],s[16],s[17],s[18],s[19]) ;
	if (num <= 0) { 
	    continue;
	}
	if(code[0] == ';'){
	    continue;

	}else if (strcmp(code, "include") == 0){
	    expand_expr_all(s[0], env);
	    if(jl_set_env_wnnrc1(env, s[0], error_handler, message_handler, level + 1) == -1){
		fclose(fp);
		return(-1);
	    }
	}else if (strcmp(code, "setdic") == 0){
	    /* dic_add */
	    int prio, rdonly, hrdonly, rev;
	    int  (*error_handler1)() = (int (*)())0;
	    expand_expr_all(s[0], env);
	    if(num < 3 || !REAL_PARAM(s[1])){
		s[1][0] = 0;
	    }else{
		expand_expr_all(s[1], env);
	    }
	    prio = (num >= 4 && REAL_PARAM(s[2]))? atoi(s[2]) : WNN_DIC_PRIO_DEFAULT;
	    rdonly = (num >= 5 && REAL_PARAM(s[3]))? atoi(s[3]) : 0;
	    hrdonly = (num >= 6 && REAL_PARAM(s[4]))? atoi(s[4]) : 0;
	    if(num < 7 || !REAL_PARAM(s[5])) s[5][0] = 0;
	    if(num < 8 || !REAL_PARAM(s[6])) s[6][0] = 0;
	    rev = (num >= 9 && REAL_PARAM(s[7]))? atoi(s[7]) : 0;

	    if(confirm_state == CONFIRM || confirm_state == CONFIRM1){
		error_handler1 = error_handler;
	    }else if (confirm_state == CREATE_WITHOUT_CONFIRM){

		error_handler1 = (int (*)())WNN_CREATE;
	    }else if (confirm_state == NO_CREATE){
		error_handler1 = (int (*)())WNN_NO_CREATE;
	    }
	    if(jl_dic_add_e(env,s[0],s[1],rev, prio,rdonly, hrdonly, s[5], s[6],
			    error_handler1, message_handler) == -1 
	       && wnn_errorno != 0){
		if (wnn_errorno != WNN_JSERVER_DEAD)
		    message_out(message_handler,
			"%s (%s) :%s\n", s[0], s[1], wnn_perror_lang(env->lang));
		else
		    message_out(message_handler, "%s\n", wnn_perror_lang(env->lang));
		goto _Err_happend;
	    }

	}else if ((strcmp(code, "setfuzokugo") == 0) ||
		  (strcmp(code, "setgrammar") == 0)){
	    /* fuzokugo_set */
	    expand_expr_all(s[0], env);
	    if(jl_fuzokugo_set_e(env,s[0]) == -1){
		if (wnn_errorno != WNN_JSERVER_DEAD)
		    message_out(message_handler,
			"%s :%s\n", s[0], wnn_perror_lang(env->lang));
		else
		    message_out(message_handler, "%s\n", wnn_perror_lang(env->lang));
		goto _Err_happend;
	    }
	} else if (strcmp(code, "setparam") == 0){
	    struct wnn_param para;
	    /* setparam --- set parameter */
	    change_ascii_to_int(s[0], &para.n);
	    change_ascii_to_int(s[1], &para.nsho);
	    change_ascii_to_int(s[2], &para.p1);
	    change_ascii_to_int(s[3], &para.p2);
	    change_ascii_to_int(s[4], &para.p3);
	    change_ascii_to_int(s[5], &para.p4);
	    change_ascii_to_int(s[6], &para.p5);
	    change_ascii_to_int(s[7], &para.p6);
	    change_ascii_to_int(s[8], &para.p7);
	    change_ascii_to_int(s[9], &para.p8);

	    change_ascii_to_int(s[10], &para.p9);
	    change_ascii_to_int(s[11], &para.p10);
	    change_ascii_to_int(s[12], &para.p11);
	    change_ascii_to_int(s[13], &para.p12);
	    change_ascii_to_int(s[14], &para.p13);
	    change_ascii_to_int(s[15], &para.p14);
	    change_ascii_to_int(s[16], &para.p15);

	    if (js_param_set(env,&para) < 0) {
		fclose(fp);
		message_out(message_handler,
		    msg_get(wnn_msg_cat, 208, NULL, env->lang), wnnrc_n);
		    /*
		    "ファイル \"%s\" で環境設定中に、エラーが発生したために、設定を中止します。\n",
		    */
		if_dead_disconnect(env, -1);
	    }
	} else if (strcmp(code, "confirm") == 0){
	    confirm_state = CONFIRM;
	}else if (strcmp(code, "confirm1") == 0){
	    confirm_state = CONFIRM1;
	}else if (strcmp(code, "create_without_confirm") == 0){
	    confirm_state = CREATE_WITHOUT_CONFIRM;
	}else if (strcmp(code, "no_create") == 0){
	    confirm_state = NO_CREATE;
	}
    }
    fclose(fp);
    return(0);

_Err_happend:
    message_out(message_handler,
		msg_get(wnn_msg_cat, 208, NULL, env->lang), wnnrc_n);
		/*
		"ファイル \"%s\" で環境設定中に、エラーが発生したために、設定を中止します。\n",
		*/
    fclose(fp);
    return(-1);
}

static int
expand_expr_all(s, env)
struct wnn_env *env;
register char *s;
{
    register char *c;

    for(c = s; *c; c++){
	if(*c == '~' || *c == '@'){
	    if(expand_expr(c, env) == -1) return(-1);
	}
    }
    return(0);
}

/* copy of js.c */
static	char *
getlogname()
{
 struct passwd *getpwuid();
 return getpwuid(getuid())->pw_name;
}


static int
expand_expr(s, env)
struct wnn_env *env;
 /**	~user、@HOME、@LIBDIR @ENV @USR の展開(但し、文字列の先頭のみ)。
   できない時は-1が
   返り、その場合sの中身は着々とそのまんま。sの長さ＜256と仮定してる。*/
register char	*s;
{
	char	*p, *s1;
	char	tmp[EXPAND_PATH_LENGTH];
	int	noerr, expandsuc;
	struct	passwd	*u;
        extern char *getenv();
	extern struct	passwd	*getpwnam();
#if defined(SYSVR2) && !defined(AIXV3) && !defined(PPC) && !defined(linux)
	extern char * strchr();
#endif

	if(*s != '~' && *s != '@') return(0);
	if((int)strlen(s) >= EXPAND_PATH_LENGTH) return(-1);

	s1 = s;
#ifdef BSD42
	if(NULL != (p = index(++s1, '/'))){
#else
	if(NULL != (p = strchr(++s1, '/'))){
#endif
		strcpy(tmp, p);
		*p = '\0';
	} else *tmp = '\0';
 /* ここまでは準備。s…先頭、s1…２文字目、p…最初の'/'のあったところ
    （ここで一旦切る）、tmp…それ以後のコピー。*/

	if(*s == '~'){
		if(*s1){
			noerr = expandsuc = 
			(NULL != (u = getpwnam(s1)) &&
			 (int)strlen(p = u -> pw_dir) + (int)strlen(tmp) <
			 EXPAND_PATH_LENGTH );

		} else {
			noerr = expandsuc =
			(NULL != (p = getenv("HOME")) &&
			 (int)strlen(p) + (int)strlen(tmp) < EXPAND_PATH_LENGTH);
		}

	} else { /* then, *s must be '@' */
		if(!strcmp(s1, "HOME")){
			noerr = expandsuc = 
			(NULL != (p = getenv("HOME")) &&
			 (int)strlen(p) + (int)strlen(tmp) < EXPAND_PATH_LENGTH);
		}else if(!strcmp(s1, "WNN_DIC_DIR")){
	                char    buf[EXPAND_PATH_LENGTH];
			expandsuc = 1;
			noerr = 
			    (NULL != (p = getenv("HOME")) &&
			     (int)strlen(p) + (int)strlen(tmp) < EXPAND_PATH_LENGTH);
			strcpy(buf, p);
			strcat(buf, "/");

			p = getenv("WNN_DIC_DIR");
			if(p){
			    strcat(buf, p);
			}else{
			    strcat(buf, "Wnn");
			}
			p = buf;
		} else if (!strcmp(s1, "LIBDIR")){
			noerr = expandsuc = 
			((int)strlen(p= LIBDIR)+ (int)strlen(tmp) < EXPAND_PATH_LENGTH);
		} else if (!strcmp(s1, "ENV")){  /* Added */
			noerr = expandsuc = 
			(NULL != (p = env_name(env)) &&
			 (int)strlen(p)+ (int)strlen(tmp) < EXPAND_PATH_LENGTH);
		}else if (!strcmp(s1, "USR")){
			noerr = expandsuc = 
			(NULL != (p = getlogname()) &&
			 (int)strlen(p)+ (int)strlen(tmp) < EXPAND_PATH_LENGTH);
		} else { /* @HOME, @LIBDIR @ENV igai ha kaenai */
			noerr = 1; expandsuc = 0;
		}
	}

	if(expandsuc) strcpy(s, p);
	strcat(s, tmp);
	return(noerr ? 0 : -1);
}

static int
change_ascii_to_int(st,dp)
register char *st;
int *dp;
{
    register int total,flag;

    total = 0;
    flag = 0;
    while(*st != NULL){
	if (isdigit(*st)){
	    total = total * 10 + (*st - '0');
	} else if (*st == '+') {
	    if (flag != 0) { return(-1); }
	    flag = 1;
	} else if (*st == '-') {
	    if (flag != 0) { return(-1); }
	    flag = -1;
	} else { return(-1); }
	st++;
    }
    if (flag == 0){
	flag = 1;
    }
    *dp = total * flag;
    return(1);
}

static int
file_exist(env, n)
struct wnn_env *env;
char *n;
{
    if(n[0] == C_LOCAL){
 	wnn_errorno = 0;
 	return(access(n + 1, 4));
     }else{
	 return(js_access(env,n,4));
    }
}    

static int
create_file(env,n, d, fid, pwd_dic, pwd_hindo, error_handler, message_handler)
register struct wnn_env *env;
char *n;
int d;
int fid;
char *pwd_dic, *pwd_hindo;
int  (*error_handler)(), (*message_handler)();
{
    char pwd[WNN_PASSWD_LEN], hpwd[WNN_PASSWD_LEN];
    int rev_dict_type;

    if(
       make_dir_rec1(env, n, error_handler, message_handler) == -1){
	wnn_errorno = WNN_MKDIR_FAIL;
	return(-1);
    }
    if(d == HINDO){
	if(create_pwd_file(env, pwd_hindo, error_handler, message_handler) == -1)return(-1);
	if(get_pwd(pwd_hindo, hpwd) == -1)return(-1);
	if(n[0] == C_LOCAL){
	    if(js_hindo_file_create_client(env, fid, n + 1, NULL, hpwd) == -1){
		message_out(message_handler, wnn_perror_lang(env->lang));
		if_dead_disconnect(env, -1);
	    }else{
		message_out(message_handler,
			    "%s \"%s\" %s",
			    msg_get(wnn_msg_cat, 203, NULL, env->lang),
			    n,
			    msg_get(wnn_msg_cat, 209, NULL, env->lang));
			    /*
			    "頻度ファイル \"%s\" を作りました。",
			    */
		chown(n + 1, getuid(), -1); /* H.T. */
		return(0);
	    }
	}else{
	    if(js_hindo_file_create(env, fid, n, NULL, hpwd) == -1){
		message_out(message_handler, wnn_perror_lang(env->lang));
		if_dead_disconnect(env, -1);
	    }else{
		message_out(message_handler,
			    "%s \"%s\" %s",
			    msg_get(wnn_msg_cat, 203, NULL, env->lang),
			    n,
			    msg_get(wnn_msg_cat, 209, NULL, env->lang));
			    /*
			    "頻度ファイル \"%s\" を作りました。",
			    */
		return(0);
	    }
	}
    }else {
	if(create_pwd_file(env, pwd_hindo, error_handler, message_handler) == -1)return(-1);
	if(get_pwd(pwd_hindo, hpwd) == -1)return(-1);
	if(create_pwd_file(env, pwd_dic, error_handler, message_handler) == -1)return(-1);
	if(get_pwd(pwd_dic, pwd) == -1)return(-1);

#ifdef	CONVERT_with_SiSheng
	if(!strncmp(js_get_lang(env), WNN_C_LANG, 5) ||
	   !strncmp(js_get_lang(env), WNN_T_LANG, 5))
	    rev_dict_type = CWNN_REV_DICT;
	else
#endif
	    rev_dict_type = WNN_REV_DICT;

	if(n[0] == C_LOCAL){
	    if(js_dic_file_create_client(env, n + 1, rev_dict_type,
						NULL, pwd, hpwd) == -1){
		message_out(message_handler, wnn_perror_lang(env->lang));
		if_dead_disconnect(env, -1);
	    }else{
		message_out(message_handler,
			    "%s \"%s\" %s",
			    msg_get(wnn_msg_cat, 200, NULL, env->lang),
			    n,
			    msg_get(wnn_msg_cat, 209, NULL, env->lang));
			    /*
			    "辞書ファイル \"%s\" を作りました。",
			    */
		chown(n + 1, getuid(), -1);
		return(0);
	    }
	}else{
	    if(js_dic_file_create(env, n, rev_dict_type, NULL, pwd, hpwd)== -1){
		message_out(message_handler, wnn_perror_lang(env->lang));
		if_dead_disconnect(env, -1);
	    }else{
		message_out(message_handler,
			    "%s \"%s\" %s",
			    msg_get(wnn_msg_cat, 200, NULL, env->lang),
			    n,
			    msg_get(wnn_msg_cat, 209, NULL, env->lang));
			    /*
			    "辞書ファイル \"%s\" を作りました。",
			    */
		return(0);
	    }
	}
    }
}

static int
make_dir_rec1(env, path, error_handler, message_handler)
struct wnn_env *env;
register char *path;
int  (*error_handler)(), (*message_handler)();
{
    char gomi[128];
    register char *c;
    for(c = path;*c;c++){
	if(*c == '/'){
	    strncpy(gomi,path,c - path);
	    gomi[c - path] = 0;
	    if(make_dir1(env, gomi, error_handler, message_handler) == -1){
		return(-1);
	    }
	}
    }
    return(0);
}

static int
make_dir1(env, dirname, error_handler, message_handler)
register struct wnn_env *env;
register char *dirname;
int  (*error_handler)(), (*message_handler)();
{
    char gomi[128];
    if(dirname[0] == C_LOCAL){
	if(*(dirname + 1) == 0) return(0);
	if(access(dirname + 1 , 0) == 0){ /* check for existence */
	    return(0); /* dir already exists */ 
	}
    }else{
	if(*dirname == 0) return(0);
	if(js_access(env, dirname , 0) == 0){ /* check for existence */
	    return(0); /* dir already exists */ 
	}
    }
    if((int)error_handler != WNN_CREATE){
	sprintf(gomi, "%s \"%s\" %s%s",
		msg_get(wnn_msg_cat, 210, NULL, env->lang),
		dirname,
		msg_get(wnn_msg_cat, 201, NULL, env->lang),
		msg_get(wnn_msg_cat, 202, NULL, env->lang));
		/*
		"directry \"%s\" が無いよ。作る?(Y/N)",
		*/
	if(call_error_handler(error_handler,gomi) == 0){
	    wnn_errorno = WNN_MKDIR_FAIL;
	    return(-1);
	}
    }
    if(dirname[0] == C_LOCAL){  /* Create Directory */
#define	MODE (0000000 | 0000777)
#if defined(BSD42) || defined(uniosu)
	if(mkdir(dirname + 1 , MODE ) != 0 ){
	    wnn_errorno=WNN_MKDIR_FAIL;
	    return(-1);
	}
#endif
#if defined(SYSVR2) && !defined(uniosu)
	char buf[256];
	strcpy(buf , "/bin/mkdir ");
	strcat(buf ,dirname + 1 );
	if(system(buf) != 0){
	    /*
	    wnn_errorno=WNN_MKDIR_FAIL;
	    return(-1);
	    */
	}
#endif
	chmod(dirname + 1,MODE);
	chown(dirname + 1, getuid(), -1);
    }else{
	if(js_mkdir(env, dirname)){
	    if_dead_disconnect(env, -1);
	}
    }
    return(0);
}


static  int
call_error_handler(error_handler, c)
int (*error_handler)();
char *c;
{
    register int x;
    x = error_handler(c);
    if(confirm_state == CONFIRM1){
	if(x) confirm_state = CREATE_WITHOUT_CONFIRM;
	else  confirm_state = NO_CREATE;
    }
    return(x);
}

static void
message_out(message_handler, format, s1, s2, s3, s4, s5, s6, s7, s8)
int (*message_handler)();
char *format;
int s1, s2, s3, s4, s5, s6, s7, s8;
{
    char buf[256];

    if(message_handler){
	sprintf(buf, format, s1, s2, s3, s4, s5, s6, s7, s8);
	(*message_handler)(buf);
    }
}


int
jl_yomi_len(buf, bun_no, bun_no2) 
struct wnn_buf *buf;
register int bun_no, bun_no2;
{
    register int len = 0;

    wnn_errorno = 0;
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0)
	bun_no2 = buf->bun_suu;
    for(;bun_no < bun_no2;bun_no++){
	len += buf->bun[bun_no]->yomilen;
    }
    return(len);
}


int
jl_kanji_len(buf, bun_no, bun_no2)
struct wnn_buf *buf;
register int bun_no, bun_no2;
{
    register int len = 0;

    wnn_errorno = 0;
    if(bun_no < 0) return(0); 
    if(bun_no2 >= buf->bun_suu || bun_no2 < 0)
	bun_no2 = buf->bun_suu;
    for(;bun_no < bun_no2;bun_no++){
	len += buf->bun[bun_no]->kanjilen;
    }
    return(len);
}

int wnn_word_use_initial_hindo = 0;

int
jl_word_use_e(env, dic_no, entry)
register struct wnn_env *env;
int dic_no, entry;
{
    register struct wnn_jdata *jd;

    wnn_errorno = 0;
    if(js_word_info(env,dic_no,entry, &rb) == -1)
	if_dead_disconnect(env, -1);
    jd = (struct wnn_jdata *)(rb.buf);
    if(jd->hindo != -1){
	if(js_hindo_set(env, dic_no, entry,WNN_IMA_OFF, WNN_ENTRY_NO_USE) == -1) {
	    if_dead_disconnect(env, -1);
	}
    }else{
	if(js_hindo_set(env, dic_no, entry,
			(wnn_word_use_initial_hindo & 0x80) ?
				WNN_IMA_ON : WNN_IMA_OFF ,
			wnn_word_use_initial_hindo & 0x7f) == -1) {
	    if_dead_disconnect(env, -1);
	}
    }
    return(0);
}    

void
jl_env_set(buf, env)
register struct wnn_env *env;
register struct wnn_buf *buf;
{
    wnn_errorno = 0;
    buf->env = env;
}


struct wnn_env *
jl_env_get(buf)
register struct wnn_buf *buf;
{

    wnn_errorno = 0;
    return(buf->env);
}


int
jl_param_set_e(env, para)
register struct wnn_env *env;
struct wnn_param *para;
{
    register int x;

    wnn_errorno = 0;
    if((x = js_param_set(env, para)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}

int
jl_param_get_e(env, para)
struct wnn_env *env;
struct wnn_param *para;
{
    register int x;
    wnn_errorno = 0;
    if((x = js_param_get(env, para)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}


int
jl_dic_use_e(env, dic_no, flag)
struct wnn_env *env;
int	dic_no,flag;
{
    register int x;
    wnn_errorno = 0;
    if((x = js_dic_use(env, dic_no, flag)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}

int
jl_word_add_e(env, dic_no, yomi, kanji, comment, hinsi, init_hindo)
struct wnn_env *env;
int	dic_no;
w_char	*yomi,*kanji,*comment;
int	hinsi,init_hindo;

{
    register int x;
    wnn_errorno = 0;
    if((x = js_word_add(env, dic_no, yomi, kanji, comment, hinsi, init_hindo)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}

int
jl_word_delete_e(env, dic_no, entry)
struct wnn_env *env;
int	dic_no;
int	entry;
{
    register int x;
    wnn_errorno = 0;
    if((x = js_word_delete(env, dic_no, entry)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}

static int
file_read(env, fname)
struct wnn_env *env;
char *fname;
{
    register int fid;
    if(fname[0] == C_LOCAL){
	fid = js_file_send(env, fname + 1);
    }else{
	fid = js_file_read(env, fname);
    }
    if(fid >= 0){
	add_file_to_env(env, fid, fname);
    }
    return(fid);
}

static int
file_remove(server, fname, pwd)
register WNN_JSERVER_ID *server;
char *fname;
char *pwd;
{
    if(fname[0] == C_LOCAL){
	return(js_file_remove_client(server, fname + 1, pwd));
    }else{
	return(js_file_remove(server, fname, pwd));
    }
}


static int
file_discard(env, fid)
register struct wnn_env *env;
register int fid;
{
    delete_file_from_env(env, fid);
    return(js_file_discard(env, fid));
}

int
jl_hinsi_number_e(env, name)
register struct wnn_env *env;
w_char *name;
{
    register int x;
    wnn_errorno = 0;
    if((x = js_hinsi_number(env->js_id, name)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}

w_char *jl_hinsi_name_e(env, no)
register struct wnn_env *env;
register int no;
{
    wnn_errorno = 0;
    if(js_hinsi_name(env->js_id, no, &rb) == -1)
	if_dead_disconnect(env, NULL);
    return ((w_char *)(rb.buf));
}

int
jl_hinsi_list_e(env, dic_no, name, area)
register struct wnn_env *env;
int dic_no;
w_char *name;
w_char ***area;
{
    int x;
    wnn_errorno = 0;
    if((x = js_hinsi_list(env, dic_no, name, &rb)) == -1)
	if_dead_disconnect(env, -1);
    *area = (w_char **)(rb.buf);
    return(x);
}

int
jl_hinsi_dicts_e(env, no, area)
register struct wnn_env *env;
int no;
int **area;
{
    int x;
    wnn_errorno = 0;
    if((x = js_hinsi_dicts(env, no, &rb)) == -1)
	if_dead_disconnect(env, -1);
    *area = (int *)(rb.buf);
    return(x);
}


int
jl_word_comment_set_e(env, dic_no, entry, comment)
register struct wnn_env *env;
int	dic_no,entry;
w_char *comment;
{
    register int x;
    wnn_errorno = 0;
    if((x = js_word_comment_set(env, dic_no, entry, comment)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}

int
jl_dic_comment_set_e(env, dic_no, comment)
register struct wnn_env *env;
int dic_no;
w_char *comment;
{
    register int x;
    WNN_DIC_INFO dic;
    WNN_FILE_INFO_STRUCT file;

    wnn_errorno = 0;
    if(js_dic_info(env,dic_no,&dic) < 0)
	    if_dead_disconnect(env, -1);
    /*	dic Body	*/
    if (js_file_info(env,dic.body,&file) < 0)
	    if_dead_disconnect(env, -1);
    if((x = js_file_comment_set(env, file.fid, comment)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}

#ifdef nodef  /*当面の間、頻度ファイルのコメントはユーザに見せない。*/
int
jl_hindo_comment_set_e(env, dic_no, comment)
register struct wnn_env *env;
int dic_no;
w_char *comment;
{
    register int x;
    WNN_DIC_INFO dic;
    WNN_FILE_INFO_STRUCT file;

    wnn_errorno = 0;
    if(js_dic_info(env,dic_no,&dic) < 0)
	    if_dead_disconnect(env, -1);
    /*	dic Body	*/
    if(dic.hindo == -1){
	wnn_errorno = WNN_NO_HINDO_FILE;
	return(-1);
    }
    if (js_file_info(env,dic.hindo,&file) < 0)
	    if_dead_disconnect(env, -1);
    if((x = js_file_comment_set(env, file.fid, comment)) == -1)
	if_dead_disconnect(env, -1);
    return (x);
}
#endif
