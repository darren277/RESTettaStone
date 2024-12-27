import { NextRequest, NextResponse } from 'next/server';

export function middleware(req) {
    if (req.nextUrl.href.includes('/nextapp/_next/')) return NextResponse.rewrite(req.nextUrl.href.replace('/nextapp/_next/', '/_next/'));
    return null;
}
