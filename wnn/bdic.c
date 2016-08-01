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
/*	Version 4.0
 */
/*
  Binary (Updatable, Stable) dictionary common routine.
*/

#if defined(JSERVER) || defined(JS)
#ifdef WRITE_CHECK
#define BDIC_WRITE_CHECK
#endif
#endif /* defined(JSERVER) || defined(JS) */

#ifdef UX386
#include <X11/Xos.h>
#else
#include <sys/types.h>
#endif
#include <unistd.h>    /* for F_OK */
#ifndef JS
#include <sys/stat.h>
#include <stdio.h>
#include "commonhd.h"
#include "jslib.h"
#include "jh.h"
#endif
#include "jdata.h"
#include "wnn_os.h"
#include "wnn_string.h"

#ifdef JSERVER
#ifndef BDIC_WRITE_CHECK
#define vputc(X, pt) ((pt)? putc((X), (pt)):xputc_cur(X))
#endif /* !BDIC_WRITE_CHECK */
#define vgetc(pt) ((pt)? getc((pt)):xgetc_cur())
extern int xgetc_cur();
#else
#ifdef JS
#ifndef BDIC_WRITE_CHECK
#define vputc(X, pt) putc((X), (pt))
#endif /* !BDIC_WRITE_CHECK */
#define vgetc(pt) getc(pt)
#else
#define vputc(X, pt) putc((X), (pt))
#define vgetc(pt) getc(pt)
#endif
#endif

#ifndef min
#define min(a, b) ((a > b)? b:a)
#define max(a, b) ((a < b)? b:a)
#endif

#ifdef JS
static
#else
extern
#endif
int output_file_header(), input_file_uniq(), output_header_jt(),
#ifdef BDIC_WRITE_CHECK
  put_short(), output_file_uniq(),
#endif /* BDIC_WRITE_CHECK */
  create_file_header(), input_file_header(), change_file_uniq1(),
  output_header_hjt();

#ifdef JS
static
#else
extern
#endif
#ifdef BDIC_WRITE_CHECK
void new_pwd();
#else /* BDIC_WRITE_CHECK */
void put_short(), output_file_uniq(), new_pwd();
#endif /* BDIC_WRITE_CHECK */

#ifdef BDIC_WRITE_CHECK
#ifdef JSERVER
extern void xputc_cur();

static int
vputc(c, fp)
char c;
FILE *fp;
{
    if (fp) {
	if (fwrite(&c, sizeof(char), 1, fp) <= 0) return(-1);
    } else {
	xputc_cur(c);
    }
    return(0);
}
#else /* JSERVER */
static int
vputc(c, fp)
char c;
FILE *fp;
{
    if (fwrite(&c, sizeof(char), 1, fp) <= 0) return(-1);
    return(0);
}
#endif /* JSERVER */
#endif /* BDIC_WRITE_CHECK */

#ifdef JS
static
#endif
#ifdef BDIC_WRITE_CHECK
int
#else /* BDIC_WRITE_CHECK */
void
#endif /* BDIC_WRITE_CHECK */
putnull(ofpter, n)
register FILE *ofpter;
register int n;
{
    for(;n;n--){
#ifdef BDIC_WRITE_CHECK
	if (vputc('\0', ofpter) == -1) return(-1);
#else /* BDIC_WRITE_CHECK */
	vputc('\0', ofpter);
#endif /* BDIC_WRITE_CHECK */
    }
#ifdef BDIC_WRITE_CHECK
    return(0);
#endif /* BDIC_WRITE_CHECK */
}

#ifdef JS
static
#endif
#ifdef BDIC_WRITE_CHECK
int
#else /* BDIC_WRITE_CHECK */
void
#endif /* BDIC_WRITE_CHECK */
put_n_str(ofpter, c, n)
register FILE *ofpter;
register int n;
register char *c;
{
    for(;n;n--){
#ifdef BDIC_WRITE_CHECK
	if (vputc(*c++, ofpter) == -1) return(-1);
#else /* BDIC_WRITE_CHECK */
	vputc(*c++, ofpter);
#endif /* BDIC_WRITE_CHECK */
    }
#ifdef BDIC_WRITE_CHECK
    return(0);
#endif /* BDIC_WRITE_CHECK */
}  

#ifdef JS
static
#endif
#ifdef BDIC_WRITE_CHECK
int
#else /* BDIC_WRITE_CHECK */
void
#endif /* BDIC_WRITE_CHECK */
put_n_EU_str(ofpter, c, n)
register FILE *ofpter;
register w_char *c;
register int n;
{
    for(;n;n--){
#ifdef BDIC_WRITE_CHECK
	if (put_short(ofpter, (int)(*c++)) == -1) return(-1);
#else /* BDIC_WRITE_CHECK */
	put_short(ofpter, (int)(*c++));
#endif /* BDIC_WRITE_CHECK */
    }
#ifdef BDIC_WRITE_CHECK
    return(0);
#endif /* BDIC_WRITE_CHECK */
}

#ifdef JS
static
#endif
#ifdef BDIC_WRITE_CHECK
int
#else /* BDIC_WRITE_CHECK */
void
#endif /* BDIC_WRITE_CHECK */
put_short(ofpter, i)
register FILE *ofpter;
register int i;
{
#ifdef BDIC_WRITE_CHECK
    if ((vputc(i >> 8, ofpter) == -1) ||
	(vputc(i     , ofpter) == -1)) return(-1);
    return(0);
#else /* BDIC_WRITE_CHECK */
    vputc(i >> 8, ofpter);
    vputc(i     , ofpter);
#endif /* BDIC_WRITE_CHECK */
}

#ifdef JS
static
#endif
#ifdef BDIC_WRITE_CHECK
int
#else /* BDIC_WRITE_CHECK */
void
#endif /* BDIC_WRITE_CHECK */
putint(ofpter, i)
register FILE *ofpter;
register int i;
{
#ifdef BDIC_WRITE_CHECK
    if ((vputc(i >> 24, ofpter) == -1) ||
	(vputc(i >> 16, ofpter) == -1) ||
	(vputc(i >> 8 , ofpter) == -1) ||
	(vputc(i      , ofpter) == -1)) return(-1);
    return(0);
#else /* BDIC_WRITE_CHECK */
    vputc(i >> 24, ofpter);
    vputc(i >> 16, ofpter);
    vputc(i >> 8 , ofpter);
    vputc(i      , ofpter);
#endif /* BDIC_WRITE_CHECK */
}

#ifdef JS
static
#endif
int
getnull(ifpter, n)
register FILE *ifpter;
register int n;
{
    register int k;
    
    for(;n;n--){
	k = vgetc(ifpter);
	if(k == -1)return(-1);
    }
    return(0);
}

#ifdef JS
static
#endif
int
getnstr(ifpter, n, st)
register FILE *ifpter;
register int n;
register char *st;
{
    int k;
    
    for(;n;n--){
	k = vgetc(ifpter);
	*st++ = k;
	if(k == -1)return(-1);
    }
    return(0);
}

#if	!defined(JS) && !defined(JSERVER)
int
get_n_EU_str(ifpter, n, st)
register FILE *ifpter;
register int n;
register w_char *st;
{
    register    int k;
    extern int get_short();
    
    for(;n;n--){
	k = get_short((short *)st++, ifpter);
	if(k == -1)return(-1);
    }
    return(0);
}

int
get_short(sp,ifpter)
register short *sp;
FILE *ifpter;
{
    register int i = 0;
    register int k;
    
    i |= (k = vgetc(ifpter)) << 8;
    if(k == -1)return(-1);
    i |= (k = vgetc(ifpter));
    if(k == -1)return(-1);
    *sp = i;
    return(0);
}
#endif

#ifdef JS
static
#endif
int
getint(ip,ifpter)
register FILE *ifpter;
int *ip;
{
    register int i = 0;
    register int k;
    
    i |= (k = vgetc(ifpter)) << 24;
    if(k == -1)return(-1);
    i |= (k = vgetc(ifpter)) << 16;
    if(k == -1)return(-1);
    i |= (k = vgetc(ifpter)) << 8;
    if(k == -1)return(-1);
    i |= (k = vgetc(ifpter));
    if(k == -1)return(-1);
    *ip = i;
    return(0);
}

#ifndef JS
void
udytoS(yomi,tnum,hostart,tary)
register w_char *yomi;
register int tnum;
char *hostart;
register struct uind1 *tary;
{
    struct uind2 *hop;
    int len;
    
    hop = (struct uind2 *)((char *)hostart + tary[tnum].pter);
    yomi[0] = tary[tnum].yomi1 >> 16;
    yomi[1] = tary[tnum].yomi1 & 0xffff;
    if(yomi[1]){
	yomi[2] = tary[tnum].yomi2 >> 16;
	if(yomi[2]){
	    yomi[3] = tary[tnum].yomi2 & 0xffff;
	}
    }
    len = hop->yomi[0];
    /*Strncpy(yomi + 4, (hop->yomi) + 1, len - 4);
      In order not to use ../etc/strings.c*/
    bcopy((char *)((hop->yomi) + 1), (char *)(yomi + 4),  max(0, ((len - 4)) * 2));
    yomi[len] = 0;
}
#endif

#ifdef BDIC_WRITE_CHECK
static char *
make_tmp_name(n)
char *n;
{
    static char buf[256];

    if (n == NULL || *n == '\0') return NULL;
    sprintf(buf, "%s~", n);
    return(buf);
}

static char *
make_backup_name(n)
char *n;
{
    static char buf[256];

    if (n == NULL || *n == '\0') return NULL;
    sprintf(buf, "%s#", n);
    return(buf);
}

#ifdef nodef
static char *
make_backup_name(n)
char *n;
{
    static char buf[256];
    char base_name[64];
    char *p;
#ifndef SYSVR2
    extern char *rindex();
#else
    extern char *strrchr();
#endif

    if (n == NULL || *n == '\0') return NULL;
    strcpy(buf, n);
#ifndef SYSVR2
    if ((p = rindex(buf, '/')) == NULL) {
#else
    if ((p = strrchr(buf, '/')) == NULL) {
#endif
	strcpy(base_name, buf);
	sprintf(buf, "#%s#", base_name);
    } else {
	p++;
	strcpy(base_name, p);
	*p = '\0';
	strcat(buf, "#");
	strcat(buf, base_name);
	strcat(buf, "#");
    }
    return(buf);
}
#endif

#ifdef JS
static
#endif
void
check_backup(n)
char *n;
{
    char *p;

    if ((p = make_tmp_name(n)) && (access(p, F_OK) != -1)) unlink(p);
    if ((p = make_backup_name(n)) && (access(p, F_OK) != -1)) {
	unlink(n);
	link(p, n);
	unlink(p);
    }
}

static int
copy_file_to_file(from, to)
FILE *from, *to;
{
    char buf[1024];
    int r_len, w_len, i;

    for (;;) {
	r_len = fread(buf, 1, 1024, from);
	if (r_len <= 0) break;
	for ( i = 0; i < r_len; i += w_len) {
	    w_len = fwrite(&buf[i], 1, r_len - i, to);
	    if (w_len <= 0) {
		fseek(from, 0, 0);
		fseek(to, 0, 0);
		return(-1);
	    }
	    if (w_len >= r_len) break;
	}
    }
    fseek(from, 0, 0);
    fseek(to, 0, 0);
    return(0);
}

#ifdef JS
static
#endif
void
delete_tmp_file(n)
char *n;
{
    if (n && *n) unlink(n);
}

#ifdef JS
static
#endif
char *
make_backup_file(n)
char *n;
{
    FILE *fp, *fp2;
    char *p, *p2;

    if ((p = make_backup_name(n)) == NULL) return(NULL);
    if ((p2 = make_tmp_name(n)) == NULL) return(NULL);
    if (((fp = fopen(p2, "w+")) == NULL) || ((fp2 = fopen(n, "r")) == NULL)) {
	return NULL;
    }
    if (copy_file_to_file(fp2, fp) == -1) {
	fclose(fp);
	fclose(fp2);
	return(NULL);
    }
    fclose(fp);
    fclose(fp2);
    if (access(p, F_OK) != -1) unlink(p);
    link(p2, p);
    unlink(p2);
    return(p);
}

#ifdef JS
static
#endif
char *
make_tmp_file(n, copy, ret_fp)
char *n;
int copy;
FILE **ret_fp;
{
    FILE *fp, *fp2;
    struct wnn_file_head fh;
    char *p;

    p = make_tmp_name(n);
    if ((fp = fopen(p, "w+")) == NULL) {
	*ret_fp = NULL;
	return NULL;
    }
    if (copy && (fp2 = fopen(n, "r")) != NULL) {
	input_file_header(fp2, &fh);
	fseek(fp2, 0, 0);
	if ((copy_file_to_file(fp2, fp) == -1) ||
	    (create_file_header(fp, fh.file_type, fh.file_passwd) == -1)) {
	    fclose(fp);
	    fclose(fp2);
	    *ret_fp = NULL;
	    return NULL;
	}
	fseek(fp, 0, 0);
	fclose(fp2);
    }
    *ret_fp = fp;
    return p;
}

#ifdef JS
static
#endif
void
move_tmp_to_org(tmp_name, org_name, copy)
char *tmp_name, *org_name;
int copy;
{
    FILE *org, *tmp;

    if (copy) {
	if (((tmp = fopen(tmp_name, "r")) == NULL)
	    || ((org = fopen(org_name, "w+")) == NULL)) {
	    return;
	}
	copy_file_to_file(tmp, org);
	fclose(tmp);
	fclose(org);
    } else {
	unlink(org_name);
	link(tmp_name, org_name);
    }
    unlink(tmp_name);
}
#endif /* BDIC_WRITE_CHECK */

#ifdef JS
static
#endif
int
create_file_header(ofpter, file_type, file_passwd)
FILE *ofpter;
int file_type;
char *file_passwd;
{
    struct stat buf;
    char hostname[WNN_HOSTLEN];
    struct wnn_file_head fh;

    if(fstat(fileno(ofpter), &buf) == -1){
	return(-1);
    }
    gethostname(hostname, WNN_HOSTLEN);
    hostname[WNN_HOSTLEN - 1] = '\0';

    fh.file_uniq.time = (int)buf.st_ctime;
    fh.file_uniq.dev = (int)buf.st_dev;
    fh.file_uniq.inode = (int)buf.st_ino;
    bzero(fh.file_uniq.createhost, WNN_HOSTLEN);
    strcpy(fh.file_uniq.createhost, hostname);
    
    /* file_uniq_org */
    fh.file_uniq_org.time = (int)buf.st_ctime;
    fh.file_uniq_org.dev = (int)buf.st_dev;
    fh.file_uniq_org.inode = (int)buf.st_ino;
    bzero(fh.file_uniq_org.createhost, WNN_HOSTLEN);
    strcpy(fh.file_uniq_org.createhost, hostname);

    fh.file_type = file_type;
    if(file_passwd){
	strncpy(fh.file_passwd, file_passwd, WNN_PASSWD_LEN);
    }else{
	bzero(fh.file_passwd, WNN_PASSWD_LEN);
    }
    if(output_file_header(ofpter, &fh) == -1) return(-1);
    return(0);
}

/* 128 Bytes File Header */

#ifdef JS
static
#endif
int
output_file_header(ofpter, hp)
FILE *ofpter;
struct wnn_file_head *hp;
{
#ifdef BDIC_WRITE_CHECK
    if ((put_n_str(ofpter, WNN_FILE_STRING, WNN_FILE_STRING_LEN) == -1) ||
	(putint(ofpter, hp->file_type) == -1) ||
	(output_file_uniq(&hp->file_uniq, ofpter) == -1) ||
	(output_file_uniq(&hp->file_uniq_org, ofpter) == -1) ||
	(put_n_str(ofpter, hp->file_passwd, WNN_PASSWD_LEN) == -1) ||
	(putnull(ofpter, 36) == -1)) return(-1);		/* Future Use */
#else /* BDIC_WRITE_CHECK */
    put_n_str(ofpter, WNN_FILE_STRING, WNN_FILE_STRING_LEN);
    putint(ofpter, hp->file_type);
    output_file_uniq(&hp->file_uniq, ofpter);
    output_file_uniq(&hp->file_uniq_org, ofpter);
    put_n_str(ofpter, hp->file_passwd, WNN_PASSWD_LEN);
    putnull(ofpter, 36);		/* Future Use */
#endif /* BDIC_WRITE_CHECK */
    return(0);
}

#ifdef JS
static
#endif
int
input_file_header(ifpter, hp)
FILE *ifpter;
struct wnn_file_head *hp;
{

    char wnn_file_string[WNN_FILE_STRING_LEN + 1];
    int err = 0;
    
    getnstr(ifpter, WNN_FILE_STRING_LEN, wnn_file_string);
    if(strncmp(wnn_file_string, WNN_FILE_STRING, WNN_FILE_STRING_LEN))
	err = -1;
    if(getint((&hp->file_type), ifpter) == -1) err = -1;
    if(input_file_uniq(&(hp->file_uniq), ifpter) == -1) err = -1;
    if(input_file_uniq(&(hp->file_uniq_org), ifpter) == -1) err = -1;
    getnstr(ifpter, WNN_PASSWD_LEN, hp->file_passwd);
    getnull(ifpter, 36);
    return(err);
}

#ifdef JS
static
#endif
#ifdef BDIC_WRITE_CHECK
int
#else /* BDIC_WRITE_CHECK */
void
#endif /* BDIC_WRITE_CHECK */
output_file_uniq(funiq, ofpter)
struct wnn_file_uniq *funiq;
FILE *ofpter;
{
#ifdef BDIC_WRITE_CHECK
    if ((putint(ofpter, funiq->time) == -1) ||
	(putint(ofpter, funiq->dev) == -1) ||
	(putint(ofpter, funiq->inode) == -1) ||
	(put_n_str(ofpter, funiq->createhost,
		   strlen(funiq->createhost)) == -1) ||
	(putnull(ofpter, WNN_HOSTLEN - strlen(funiq->createhost)) == -1))
	return(-1);
    return(0);
#else /* BDIC_WRITE_CHECK */
    putint(ofpter, funiq->time);
    putint(ofpter, funiq->dev);
    putint(ofpter, funiq->inode);
    put_n_str(ofpter, funiq->createhost, strlen(funiq->createhost));
    putnull(ofpter, WNN_HOSTLEN - strlen(funiq->createhost));
#endif /* BDIC_WRITE_CHECK */
}

#ifdef JS
static
#endif
int
input_file_uniq(funiq, ifpter)
struct wnn_file_uniq *funiq;
FILE *ifpter;
{
    if(
       getint(&(funiq->time), ifpter) == -1 ||
       getint(&(funiq->dev), ifpter) == -1 ||
       getint(&(funiq->inode), ifpter) == -1 ||
       getnstr(ifpter, WNN_HOSTLEN, funiq->createhost) == -1)
	return(-1);
    return(0);
}

#ifdef JS
static
#endif
int
check_inode(f, fh)
FILE *f;
struct wnn_file_head *fh;
{
    struct stat buf;
    if(fstat(fileno(f), &buf) == -1){
	return(-1);
    }
    if((int)buf.st_ino != fh->file_uniq.inode){
	return(-1);
    }
    return(0);
}

#ifdef JS
static
#endif
int
change_file_uniq(fh, n)
struct wnn_file_head *fh;
char *n;
{
    int x;
    FILE *fp;
#ifdef BDIC_WRITE_CHECK
    char *tmp, *backup = NULL;
    backup = make_backup_file(n);
    if ((tmp = make_tmp_file(n, 1, &fp)) == NULL) {
	delete_tmp_file(backup);
#else /* BDIC_WRITE_CHECK */
    if((fp = fopen(n,"r+")) == NULL){
#endif /* BDIC_WRITE_CHECK */
	return(-1);
    }
#ifdef BDIC_WRITE_CHECK
    if (change_file_uniq1(fp, fh->file_type, fh->file_passwd, &(fh->file_uniq)) == -1) {
	fclose(fp);
	delete_tmp_file(tmp);
	delete_tmp_file(backup);
	return(-1);
    }
#else /* BDIC_WRITE_CHECK */
    change_file_uniq1(fp, fh->file_type, fh->file_passwd, &(fh->file_uniq));
#endif /* BDIC_WRITE_CHECK */
    fseek(fp, 0, 0);
    x = input_file_header(fp, fh);/* It must not occur. */
    fclose(fp);
#ifdef BDIC_WRITE_CHECK
    if(x == -1) {
	delete_tmp_file(tmp);
	delete_tmp_file(backup);
	return(-1); 
    }
    move_tmp_to_org(tmp, n, 0);
    delete_tmp_file(backup);
#else /* BDIC_WRITE_CHECK */
    if(x == -1) return(-1);
#endif /* BDIC_WRITE_CHECK */
    return(0);
}    

#ifdef JS
static
#endif
int
change_file_uniq1(ofpter, file_type, file_passwd, file_uniq)
FILE *ofpter;
int file_type;
char *file_passwd;
struct wnn_file_uniq *file_uniq;
{
    struct stat buf;
    char hostname[WNN_HOSTLEN];
    struct wnn_file_head fh;

    if(fstat(fileno(ofpter), &buf) == -1){
	return(-1);
    }
    gethostname(hostname, WNN_HOSTLEN);
    hostname[WNN_HOSTLEN - 1] = '\0';

    fh.file_uniq.time = (int)buf.st_ctime;
    fh.file_uniq.dev = (int)buf.st_dev;
    fh.file_uniq.inode = (int)buf.st_ino;
    bzero(fh.file_uniq.createhost, WNN_HOSTLEN);
    strcpy(fh.file_uniq.createhost, hostname);
    
    /* file_uniq_org */
    fh.file_uniq_org.time = file_uniq->time;
    fh.file_uniq_org.dev = file_uniq->dev;
    fh.file_uniq_org.inode = file_uniq->inode;
    bzero(fh.file_uniq_org.createhost, WNN_HOSTLEN);
    strcpy(fh.file_uniq_org.createhost, file_uniq->createhost);

    fh.file_type = file_type;
    if(file_passwd){
	strncpy(fh.file_passwd, file_passwd, WNN_PASSWD_LEN);
    }else{
	bzero(fh.file_passwd, WNN_PASSWD_LEN);
    }
    if(output_file_header(ofpter, &fh) == -1) return(-1);
    return(0);
}



#ifdef JSERVER
int
f_uniq_cmp(a,b)
char a[],b[];
{
    return  bcmp(a,b,sizeof(struct wnn_file_uniq));
}
#endif

#if	!defined(JS) && !defined(JSERVER)
void
vputs(c, fp)
register char *c;
register FILE *fp;
{
    while(*c){
	vputc(*c++, fp);
    }
}

int
vputws(w, fp)
register w_char *w;
register FILE *fp;
{
    register int n;
    UCHAR tmp[LENGTHYOMI*3];
    
    n = wnn_sStrcpy(tmp, w);
    vputs(tmp, fp);
    return(n);
}
#endif


extern char *wnn_get_hinsi_name();

#if	!defined(JS) && !defined(JSERVER)
int
put_yomi_str(yomi, ofpter)
w_char *yomi;
FILE *ofpter;
{
    register int c;
    register int i = 0;
    UCHAR tmp[LENGTHYOMI*3], *p;

    wnn_sStrcpy(tmp, yomi);
    for(p = tmp;*p;p++){
	c = (int)*p;
	if(c == '\\'){
	    vputc('\\', ofpter);
	    vputc('\\', ofpter);
	    i += 2;
	}else if(c > 0x20){
	    vputc(c, ofpter);
	    i += 1;
	}else{
	    char tmp[8];
	    sprintf(tmp , "\\0%o\\" , c);
	    vputs(tmp, ofpter);
	    i += strlen(tmp);
	}
    }
    return(i);
}
#endif

#ifndef JS
void
Get_knj2(kptr,kanji2, kouho, yomi, comment)
UCHAR *kptr;
int kanji2;
w_char *kouho, *comment, *yomi;
{
	extern void get_kanji_str();

	int tcnt;
	for (tcnt = 0; tcnt < kanji2; tcnt++){
		kptr += *kptr;
	}
	get_kanji_str(kptr, kouho, yomi, comment);
	return;
}

void
get_kanji_str(kptr, kanji, yomi, comment)
UCHAR *kptr;
w_char *kanji, *comment, *yomi;
{
    w_char *tmpy;
    w_char *tmpk;
    w_char *tmpc;
    extern void Get_kanji_str_r();

    Get_kanji_str_r(kptr, &tmpk, &tmpy, &tmpc);

#ifdef CONVERT_from_TOP /* Don't warry. Only use in server and jutil */
    if(kanji && tmpk)wnn_Strcpy(kanji, tmpk);
    if(yomi && tmpy)wnn_Strcpy(yomi, tmpy);
#else	/* conver from bottom */
    if(kanji && tmpk)wnn_Sreverse(kanji, tmpk);
    if(yomi && tmpy)wnn_Sreverse(yomi, tmpy);
#endif	/* CONVERT_from_TOP */
    if(comment && tmpc)wnn_Strcpy(comment, tmpc);
}
    

void
Get_kanji_str_r(kptr, tmpk, tmpy, tmpc)
UCHAR *kptr;
w_char **tmpk, **tmpy, **tmpc;
{
    int state = *(kptr + 1);
    static w_char dmy = 0; /* Must not be allocated on Stack!! */
    int adres;

    if(state & FORWARDED){
	/* Forward occures when comment-set is used */
	adres = (*(w_char *)(kptr + 2)) << 16 | (*(w_char *)(kptr + 4));
	Get_kanji_str_r(kptr + adres, tmpk, tmpy, tmpc);
	return;
    }
/*    get_kanji_str_r(kptr, tmpk, tmpy, comment); */
    if(state & HAS_YOMI){
	*tmpy = ((w_char *)(kptr + 2));
	*tmpk = *tmpy + wnn_Strlen(*tmpy) + 1;
    }else{
	*tmpy = &dmy;
	*tmpk = ((w_char *)(kptr + 2));
    }
    if(state & HAS_COMMENT){
	*tmpc = *tmpk + wnn_Strlen(*tmpk) + 1;
    }else{
	*tmpc = &dmy;
    }
}

#ifdef nodef
int
Get_kanji_len(kptr, which)
UCHAR *kptr;
int which;
{
    w_char tmp[LENGTHYOMI];

    if(which == D_KANJI){
	get_kanji_str_r(kptr, tmp, NULL, NULL);
    }else{
	get_kanji_str_r(kptr, NULL, tmp, NULL);
    }	
    return(wnn_Strlen(tmp));
}

void
get_kanji_str_r(kptr, kanji, yomi, comment)
UCHAR *kptr;
w_char *kanji, *comment, *yomi;
{
    w_char *c;
    int n , k;
    
    n = *kptr++ - 1;
    c = kanji;
    if(comment)*comment = 0;
    if(yomi)*yomi = 0;
    for(k = 0 ; k < n ;) {
	if(*kptr == DIC_COMMENT_CHAR){
	    if(c) *c = 0;
	    c = comment;
	    kptr++;
	    k++;
	}else if(*kptr == DIC_YOMI_CHAR){
	    if(c) *c = 0;
	    c = yomi;
	    kptr++;
	    k++;
	}else if(*kptr & 0x80){  /* kanji char */
	    if(c) *c++ = ((int) *kptr << 8 | *(kptr + 1));
	    kptr += 2;
	    k+= 2;
	}else{
	    if(c) *c++ = *kptr;
	    kptr++;
	    k++;
	}
    }
    if(c)*c = 0;
}
#endif

UCHAR
kanjiaddr(d0, kanji, yomi, comment)
UCHAR *d0;
w_char *kanji, *yomi, *comment;
{
    w_char *dest = (w_char *)(d0 + 2);
    int state = 0;
    w_char *pt;

    if(yomi && *yomi){
#ifdef CONVERT_from_TOP /* Don't warry. Only use in server and jutil */
        pt = yomi;
        for(;*pt;){
            *dest++ = *pt++;
        } 
#else	/* conver from bottom */
	pt = yomi + wnn_Strlen(yomi) - 1;
	for(;pt >= yomi;){
	    *dest++ = *pt--;
	}
#endif /* CONVERT_from_TOP */
	state |= HAS_YOMI;
	*dest++ = 0;
    }

#ifdef CONVERT_from_TOP /* Don't warry. Only use in server and jutil */
    pt = kanji;
    for(;*pt;){
            *dest++ = *pt++;
        } 
#else	/* conver from bottom */
    pt = kanji + wnn_Strlen(kanji) - 1;
    for(;pt >= kanji;){
	*dest++ = *pt--;
    }
#endif /* CONVERT_from_TOP */
    *dest++ = 0;

    if(comment && *comment){
	pt = comment;
	for(;*pt;){
	    *dest++ = *pt++;
	}
	state |= HAS_COMMENT;
	*dest++ = 0;
    }
    *d0 = (UCHAR)((UCHAR *)dest - d0);
    *(d0 + 1) = state;
    return(*d0);
}	

#endif


#ifdef JS
static
#endif
int
create_null_dic(fn,comm, passwd, hpasswd, which)
char  *fn;
w_char *comm;
char *passwd, *hpasswd;  /* not encoded passwd */
int which;
{
    FILE *fp;
    struct JT jt;
    char epasswd[WNN_PASSWD_LEN];
    extern void new_pwd();
    
    jt.total = 0;
    jt.gosuu = 0;
    if(hpasswd){
	new_pwd(hpasswd, jt.hpasswd);
    }else{
	bzero(jt.hpasswd, WNN_PASSWD_LEN);
    }
    jt.syurui = which;
    jt.maxserial = 0;
    jt.maxtable = 0;
    jt.maxhontai = (which == WNN_UD_DICT)? 4: 0;
    jt.maxkanji = 0;
    if(comm){
	jt.maxcomment = wnn_Strlen(comm);
    }else{
	jt.maxcomment = 0;
    }
    jt.maxhinsi_list = 0;
    jt.maxri1[D_YOMI] = 0;
    jt.maxri1[D_KANJI] = 0;
    jt.maxri2 = 0;

    if((fp = fopen(fn, "w")) == NULL){
	/*	error1( "Jserver:create_null_ud:No file %s.\n", fn); */
	return(-1);
    }
    if(passwd){
	new_pwd(passwd, epasswd);
    }else{
	bzero(epasswd, WNN_PASSWD_LEN);
    }
    if(create_file_header(fp, WNN_FT_DICT_FILE,epasswd) == -1 ||
       output_header_jt(fp, &jt) == -1){
	fclose(fp);return(-1);
    }
#ifdef BDIC_WRITE_CHECK
    if (put_n_EU_str(fp, comm, jt.maxcomment) == -1) {
	fclose(fp);
	return(-1);
    }
#else /* BDIC_WRITE_CHECK */
    put_n_EU_str(fp, comm, jt.maxcomment);
#endif /* BDIC_WRITE_CHECK */
    if(which == WNN_UD_DICT){
#ifdef BDIC_WRITE_CHECK
	if (putint(fp, 0) == -1) {	/* hontai[0] */
	    fclose(fp);
	    return(-1);
	}
#else /* BDIC_WRITE_CHECK */
	putint(fp, 0);			/* hontai[0] */
#endif /* BDIC_WRITE_CHECK */
    }

#ifdef	BSD42
    fchmod(fileno(fp), 0664);
    fclose(fp);
#else	/* SYSV */
    fclose(fp);
    chmod(fn, 0664);
#endif	/* BSD42 */
    return(0);
}


#ifdef JS
static
#endif
int
create_hindo_file(funiq, fn, comm, passwd, serial)
struct wnn_file_uniq *funiq;
char *fn;
w_char *comm;
char *passwd;			/* Not encoded */
int serial;
{
    FILE *fp;
    struct HJT hjt;
    char epasswd[WNN_PASSWD_LEN];
    w_char tmp[1];

    tmp[0] = 0;
    if(comm == NULL) comm = tmp;
    bcopy(funiq, &(hjt.dic_file_uniq), WNN_F_UNIQ_LEN);
    hjt.maxcomment = wnn_Strlen(comm);
    
    hjt.maxserial = serial;

    if((fp = fopen(fn, "w")) == NULL){
	return(-1);
    }
    if(passwd){
	new_pwd(passwd, epasswd);
    }else{
	bzero(epasswd, WNN_PASSWD_LEN);
    }
    if(create_file_header(fp, WNN_FT_HINDO_FILE, epasswd) == -1){fclose(fp);return(-1);}
#ifdef BDIC_WRITE_CHECK
    if ((output_header_hjt(fp, &hjt) == -1) ||
	(put_n_EU_str(fp, comm, hjt.maxcomment) == -1) ||
	(putnull(fp, serial) == -1)) {
	fclose(fp);
	return(-1);
    }
#else /* BDIC_WRITE_CHECK */
    output_header_hjt(fp, &hjt);
    put_n_EU_str(fp, comm, hjt.maxcomment);
    putnull(fp, serial);
#endif /* BDIC_WRITE_CHECK */

#ifdef	BSD42
    fchmod(fileno(fp), 0664);
    fclose(fp);
#else	/* SYSV */
    fclose(fp);
    chmod(fn, 0664);
#endif	/* BSD42 */
    return(0);
}

/* Header Total 128 Bytes */

#ifndef	JS
int
input_header_jt(ifpter, jt1)
FILE *ifpter;
struct JT *jt1;
{
    if(
       getint(&jt1->syurui , ifpter) == -1 ||
       getint(&jt1->maxcomment, ifpter) == -1 ||
       getint(&jt1->maxhinsi_list, ifpter) == -1 ||
       getint(&jt1->maxserial , ifpter) == -1 ||
       getint(&jt1->maxkanji , ifpter) == -1 ||
       getint(&jt1->maxtable , ifpter) == -1 ||
       getint(&jt1->maxhontai , ifpter) == -1 ||
       getint(&jt1->gosuu , ifpter) == -1 ||
       getnstr(ifpter, WNN_PASSWD_LEN, jt1->hpasswd) == -1 ||
       getint(&jt1->total , ifpter) == -1 ||
       getint(&jt1->maxri1[D_YOMI] , ifpter) == -1 ||
       getint(&jt1->maxri1[D_KANJI] , ifpter) == -1 ||
       getint(&jt1->maxri2, ifpter) == -1 ||
       getnull(ifpter, 56 ) == -1)
	return(-1);
    return(0);
}
#endif

#ifdef JS
static
#endif
int
output_header_jt(ofpter, jt1)
FILE *ofpter;
struct JT *jt1;
{
#ifdef BDIC_WRITE_CHECK
    if ((putint(ofpter, jt1->syurui) == -1) ||
	(putint(ofpter, jt1->maxcomment) == -1) ||
	(putint(ofpter,  jt1->maxhinsi_list) == -1) ||
	(putint(ofpter, jt1->maxserial) == -1) ||
	(putint(ofpter, jt1->maxkanji) == -1) ||
	(putint(ofpter, jt1->maxtable) == -1) ||
	(putint(ofpter, jt1->maxhontai) == -1) ||
	(putint(ofpter, jt1->gosuu) == -1) ||
	(put_n_str(ofpter, jt1->hpasswd, WNN_PASSWD_LEN) == -1) ||
	(putint(ofpter, jt1->total) == -1) ||
	(putint(ofpter, jt1->maxri1[D_YOMI]) == -1) ||
	(putint(ofpter, jt1->maxri1[D_KANJI]) == -1) ||
	(putint(ofpter, jt1->maxri2) == -1) ||
	(putnull(ofpter, 56) == -1)) return(-1);
#else /* BDIC_WRITE_CHECK */
    putint(ofpter, jt1->syurui);
    putint(ofpter, jt1->maxcomment);
    putint(ofpter,  jt1->maxhinsi_list);
    putint(ofpter, jt1->maxserial);
    putint(ofpter, jt1->maxkanji);
    putint(ofpter, jt1->maxtable);
    putint(ofpter, jt1->maxhontai);
    putint(ofpter, jt1->gosuu);
    put_n_str(ofpter, jt1->hpasswd, WNN_PASSWD_LEN);
    putint(ofpter, jt1->total);
    putint(ofpter, jt1->maxri1[D_YOMI]);
    putint(ofpter, jt1->maxri1[D_KANJI]);
    putint(ofpter, jt1->maxri2);
    putnull(ofpter, 56);
#endif /* BDIC_WRITE_CHECK */
    return(0);
}

/* Header 64 Byte */
#ifndef	JS
int
input_header_hjt(ifpter, hjt1)
FILE *ifpter;
struct HJT *hjt1;
{
    if(
       input_file_uniq(&hjt1->dic_file_uniq, ifpter) == -1 ||  /* 7 * 4 */
       getint(&hjt1->maxcomment, ifpter) == -1 ||
       getint(&hjt1->maxserial, ifpter) == -1 ||
       getnull(ifpter, 28) == -1)  {
	return(-1);
    }
    return(0);
}
#endif

#ifdef JS
static
#endif
int
output_header_hjt(ofpter, hjt1)
FILE *ofpter;
struct HJT *hjt1;
{
#ifdef BDIC_WRITE_CHECK
    if ((output_file_uniq(&hjt1->dic_file_uniq, ofpter) == -1) ||
	(putint(ofpter, hjt1->maxcomment) == -1) ||
	(putint(ofpter, hjt1->maxserial) == -1) ||
	(putnull(ofpter, 28) == -1)) return(-1);
#else /* BDIC_WRITE_CHECK */
    output_file_uniq(&hjt1->dic_file_uniq, ofpter);
    putint(ofpter, hjt1->maxcomment);
    putint(ofpter, hjt1->maxserial);
    putnull(ofpter, 28);
#endif /* BDIC_WRITE_CHECK */
    return(0);
}


#if	!defined(JS) && !defined(JSERVER)
/* Only use JUTIL */
void
Print_entry(yomi, kstr, cstr, hindo, ima, hinsi, serial, ofpter, esc_exp)
w_char *yomi, *kstr,*cstr;
int serial, hindo, ima, hinsi;
FILE *ofpter;
int esc_exp;
{
    register int len;
    char *k;
    char buf[32];
    static w_char revy[LENGTHKANJI];
    extern void kanji_esc_str();
    
    if(serial != -1){
	sprintf(buf, "%d\t", serial);
	vputs(buf, ofpter);
    }

    len = put_yomi_str(yomi, ofpter);
    if(len < 8)vputc('\t' , ofpter);
    if(len < 16)vputc('\t' , ofpter);
    vputc('\t' , ofpter);
    
    if(esc_exp){
#ifdef CONVERT_from_TOP /* Don't warry. Only use in jutil */
	wnn_Strcpy(revy, yomi);
#else	/* conver from bottom */
	wnn_Sreverse(revy, yomi);
#endif /* CONVERT_from_TOP */
	kanji_esc_str(kstr, revy, wnn_Strlen(yomi));
	len = put_yomi_str(kstr, ofpter);
    }else{
	len = vputws(kstr, ofpter); 
    }
    if(len < 8)vputc('\t' , ofpter);
    if(len < 16)vputc('\t' , ofpter);
    vputc('\t' , ofpter);
    
    k = wnn_get_hinsi_name(hinsi);
    if(k){
	vputs(k, ofpter);
	if((int)strlen(k) < 8)vputc('\t' , ofpter);
	vputc('\t' , ofpter);
    }else{
	sprintf(buf, "%d\t\t", hinsi);
	vputs(buf, ofpter);
    }
    
    if(ima) vputc('*', ofpter);
    if(hindo == -1){  	/*  Real hindo == -1 means Not to use it */
	vputs("-", ofpter);
    }else{
	sprintf(buf, "%d", hindo);
	vputs(buf, ofpter);
    }	
    if(cstr && cstr[0]){
	vputc('\t', ofpter);
	len = vputws(cstr, ofpter); 
    }
    vputc('\n', ofpter);
}
#endif

#ifdef BDIC_WRITE_CHECK
#undef BDIC_WRITE_CHECK
#endif
