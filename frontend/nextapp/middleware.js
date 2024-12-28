import { NextRequest, NextResponse } from 'next/server';

export function middleware(req) {
    console.log('[Next.js Middleware]', req.method, req.nextUrl.pathname)
    //if (req.nextUrl.href.includes('/nextapp/_next/')) return NextResponse.rewrite(req.nextUrl.href.replace('/nextapp/_next/', '/_next/'));
    if (req.nextUrl.pathname.startsWith('/nextapp/_next')) {
        return NextResponse.rewrite(new URL(req.nextUrl.pathname.replace('/nextapp/_next', '/_next'), req.url));
    }
    return NextResponse.next();
}
