module DbDef

#region Ddl
DdlTrade() = """
create table if not exists Trade(tid serial primary key, tsCreated Int not null, status Text not null, targetDate Date not null, primitDir decimal(10,2) not null)
"""

DdlTradeMeta() = """
create table if not exists TradeMeta(tid Int primary key,
    underBid decimal(10,3) not null, underAsk decimal(10,3) not null,
    foreign key(tid) references Trade(tid) on delete cascade)
"""

DdlLegTrade() = """
create table if not exists LegTrade(lid serial primary key, tid Int not null,
    style Int not null, expiration Date not null, strike decimal(10,3) not null, side Int not null, quantity decimal(10,3) not null,
    foreign key(tid) references Trade(tid) on delete cascade)
"""

DdlLegTradeMeta() = """
create table if not exists LegTradeMeta(lid Int primary key,
    bid decimal(10,3) not null, ask decimal(10,3) not null, iv float not null,
    foreign key(lid) references LegTrade(lid) on delete cascade)
"""

DdlOrd() = """
create table if not exists Ord(oid Int not null primary key, symbol Text not null, class Text not null, orderType Int not null,
    status String not null, primitDir decimal(10,2), prillDir decimal(10,3) not null,
    tsCreated Int not null, tsFilled Int not null)
"""

DdlLegOrd() = """
create table if not exists LegOrd(olid Int not null primary key, oid Int not null, act Int not null,
    style Int not null, expiration Date not null, strike decimal(10,3) not null, side Int not null, quantity decimal(10,3) not null,
    prillDir decimal(10,3) not null,
    tsCreated Int not null, tsFilled Int not null,
    foreign key(oid) references Ord(oid) on delete cascade)
"""

DdlLegUsed() = """
create table if not exists LegUsed(lid Int not null, olid Int not null, act Int not null, quantity decimal(10,3) not null,
    foreign key(lid) references LegTrade(lid), foreign key(olid) references LegOrd(olid), primary key(lid, olid, act))
"""
#endregion

#region VLegTrade
VLegUsed() = """
create view if not exists VLegUsed as
select lu.lid, lu.olid, lu.act, lu.quantity, lo.prillDir, lo.tsCreated, lo.tsFilled from LegUsed lu join LegOrd lo on lo.olid=lu.olid
"""

VLegFilled() = """
create view if not exists VLegFilled as
select lt.lid, lt.tid, lt.style, lt.expiration, lt.strike, lt.side, lt.quantity, coalesce(m.qty,0) qtyUsed, (m.prillDirTot / m.qty) prillDir, m.tsCreated, m.tsFilled, dir.act
from LegTrade lt cross join (select 1 act union select -1 act) dir left outer join (
	select lid, act, sum(quantity) qty, sum(quantity * prillDir) prillDirTot, max(tsCreated) tsCreated, max(tsFilled) tsFilled from VLegUsed group by lid, act
) m on m.lid=lt.lid and m.act = dir.act
"""
# select lt.*, m.qty qtyUsed, (m.prillDirTot / m.qty) prillDir, m.tsCreated, m.tsFilled, m.act
# from LegTrade lt left outer join
#     (select lid, act, sum(quantity) qty, sum(quantity * prillDir) prillDirTot, max(tsCreated) tsCreated, max(tsFilled) tsFilled
#      from VLegUsed group by lid, act) m on m.lid=lt.lid

VLegTrade() = """
create view if not exists VLegTrade as
select lt.lid, lt.tid, lt.style, lt.expiration, lt.strike, lt.side, lt.quantity, ltm.bid, ltm.ask, ltm.iv, mo.pd prillDirOpen, mc.pd prillDirClose, mo.tsCreated tsCreated, mo.tsFilled tsFilled, mc.tsFilled tsClosed,
    case when mo.pd is null then 'Accepted' when mc.pd is null then 'Filled' else 'Closed' end as status
from LegTrade lt join LegTradeMeta ltm on ltm.lid=lt.lid left outer join
    (select lfo.lid, lfo.prillDir pd, lfo.tsCreated, lfo.tsFilled from VLegFilled lfo where act=1) mo on mo.lid=lt.lid left outer join
    (select lfc.lid, lfc.prillDir pd, lfc.tsFilled from VLegFilled lfc where act=-1) mc on mc.lid=lt.lid
"""
#endregion

#region VTrade
VTrade() = """
create view VTrade as
select t.tid, t.tsCreated tradeCreated, t.primitDir, t.targetDate, tm.underBid, tm.underAsk,
    mo.pd prillDirOpen, mc.pd prillDirClose, mo.tsCreated tsCreated, mo.tsFilled tsFilled, mc.tsFilled tsClosed,
    case when mc.qtyUsed = mc.quantity then 'Closed' when mc.qtyUsed > 0 then 'PartialClosed' when mo.qtyUsed = mo.quantity then 'Filled' when mo.qtyUsed > 0 then 'PartialFilled' else 'Starting' end as status
from Trade t join TradeMeta tm on t.tid=tm.tid left outer join
    lateral (select tid, sum(quantity) quantity, sum(qtyUsed) qtyUsed, sum(quantity * prillDir) pd, max(tsCreated) tsCreated, max(tsFilled) tsFilled from VLegFilled lfo where act=1 and tid=t.tid group by tid) mo on mo.tid=t.tid left outer join
    lateral (select tid, sum(quantity) quantity, sum(qtyUsed) qtyUsed, sum(quantity * prillDir) pd, max(tsFilled) tsFilled from VLegFilled lfc where act=-1 and tid=t.tid group by tid) mc on mc.tid=t.tid
"""
# create view VTrade as
# select t.tid, t.tsCreated, t.primitDir, tm.underBid, tm.underAsk,
#     mo.pd prillDirOpen, mc.pd prillDirClose, mo.tsCreated tsCreated, mo.tsFilled tsFilled, mc.tsFilled tsClosed,
#     case when mc.qtyUsed = mc.quantity then 'Closed' when mc.qtyUsed > 0 then 'PartialClosed' when mo.qtyUsed = mo.quantity then 'Filled' when mo.qtyUsed > 0 then 'PartialFilled' else 'Starting' end as status
# from Trade t join TradeMeta tm on t.tid=tm.tid left outer join
#     (select tid, sum(quantity) quantity, sum(qtyUsed) qtyUsed, sum(quantity * prillDir) pd, max(tsCreated), max(tsFilled) from VLegFilled lfo where act=1 and tid=t.tid group by tid) mo on mo.tid=t.tid left outer join
#     (select tid, sum(quantity) quantity, sum(qtyUsed) qtyUsed, sum(quantity * prillDir) pd, max(tsFilled) from VLegFilled lfc where act=-1 and tid=t.tid group by tid) mc on mc.tid=t.tid
# group by t.tid
# --where status not in ("Starting", "Canceled")

#   -- (select tid, sum(quantity) quantity from LegTrade) left outer join
# (select tid, sum(quantity) qty, sum(quantity * prillDir) prillDirTot, max(tsCreated) tsCreated, max(tsFilled) tsFilled from VLegFilledOpen lfo group by lfo.tid) mo on mo.tid=t.tid left outer join
# (select tid, sum(quantity) qty, sum(quantity * prillDir) prillDirTot, max(tsCreated) tsCreated, max(tsFilled) tsFilled from VLegFilledOpen lfo group by lfo.tid) mc on mc.tid=t.tid
#endregion

#region FindingMatches
# VFindMatches() = """
# create view VFindMatches as
# select tl.lid, ol.olid, ol.act
# from LegTrade tl, LegOrd ol
# where ol.style=tl.style and ol.expiration=tl.expiration and ol.strike=tl.strike and ol.side=(ol.act * tl.side)
# """

VUnusedLegOrd() = """
create view VUnusedLegOrd as
select lo.olid, lo.oid, lo.act, lo.style, lo.expiration, lo.strike, lo.side, lo.quantity, lo.prillDir, lo.tsCreated, lo.tsFilled, (lo.quantity - coalesce(m.qty,0)) remain
from LegOrd lo
    left outer join (select olid, sum(quantity) qty from VLegUsed group by olid) m on m.olid=lo.olid
where coalesce(m.qty,0) < lo.quantity
"""

VFindMatchesUnused() = """
create view VFindMatchesUnused as
select lt.tid, lt.lid, lo.olid, lo.act, lo.prillDir, least(lt.quantity, lo.remain) quantity
from VLegFilled lt, VUnusedLegOrd lo
where lt.style=lo.style and lt.expiration=lo.expiration and lt.strike=lo.strike and lt.side=(lo.act * lo.side)
    and lt.act=lo.act and lt.quantity > lt.qtyUsed
"""
#endregion

DDL() = [
    DdlTrade(),
    DdlTradeMeta(),
    DdlLegTrade(),
    DdlLegTradeMeta(),
    DdlOrd(),
    DdlLegOrd(),
    DdlLegUsed(),
    VLegUsed(),
    VLegFilled(),
    VLegTrade(),
    VTrade(),
    VUnusedLegOrd(),
    VFindMatchesUnused()
]

Inserts() =
    ["""insert into Ord(oid, symbol, class, orderType, status, prillDir, tsCreated, tsFilled) values(100, 'ANY', 'option', 1, 'Expired', 0, 0, 0);""",
     """insert into LegOrd(olid, oid, act, style, expiration, strike, side, quantity, prillDir, tsCreated, tsFilled) values (100, 100, -1, 0, cast(0 as date), 0, 0, 0, 0, 0, 0);"""]
# # Then for each new one:
# # update("insert into LegMatched (lid, olid, quantity, act) values (?, ?, ?, ?)", [lid, 10, qtyExp, -1])
# #  -- Don't want to do this because primary key olid would require all new entries for every one and want tsTransact, but can use expiration for tsTransact

# insert into Ord (oid, symbol, class, orderType, status, primitDir, prillDir, tsCreated, tsFilled) values (?, ?, ?, ?, ?, ?, ?, ?, ?)
# insert into LegOrd (olid, oid, act, style, expiration, strike, side, quantity, prillDir, tsCreated, tsFilled) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)

end


#===
Migrate to date column types:
SET enable_experimental_alter_column_type_general = true

-- drop view vtrade
-- drop view vlegtrade
drop view vlegfilled cascade
alter table LegTrade alter expiration type Date using cast(expiration//(24*60*60*1000) as date);

drop view vunusedlegord cascade
alter table LegOrd alter expiration type Date using cast(expiration//(24*60*60*1000) as date)

create view if not exists VLegFilled as
select lt.lid, lt.tid, lt.style, lt.expiration, lt.strike, lt.side, lt.quantity, coalesce(m.qty,0) qtyUsed, (m.prillDirTot / m.qty) prillDir, m.tsCreated, m.tsFilled, dir.act
from LegTrade lt cross join (select 1 act union select -1 act) dir left outer join (
	select lid, act, sum(quantity) qty, sum(quantity * prillDir) prillDirTot, max(tsCreated) tsCreated, max(tsFilled) tsFilled from VLegUsed group by lid, act
) m on m.lid=lt.lid and m.act = dir.act

create view if not exists VLegTrade as
select lt.lid, lt.tid, lt.style, lt.expiration, lt.strike, lt.side, lt.quantity, ltm.bid, ltm.ask, ltm.iv, mo.pd prillDirOpen, mc.pd prillDirClose, mo.tsCreated tsCreated, mo.tsFilled tsFilled, mc.tsFilled tsClosed,
    case when mo.pd is null then 'Accepted' when mc.pd is null then 'Filled' else 'Closed' end as status
from LegTrade lt join LegTradeMeta ltm on ltm.lid=lt.lid left outer join
    (select lfo.lid, lfo.prillDir pd, lfo.tsCreated, lfo.tsFilled from VLegFilled lfo where act=1) mo on mo.lid=lt.lid left outer join
    (select lfc.lid, lfc.prillDir pd, lfc.tsFilled from VLegFilled lfc where act=-1) mc on mc.lid=lt.lid

alter table Trade add column targetDate Date;
update Trade set targetDate = lt.expiration from (select tid, min(expiration) as expiration from VLegTrade group by tid) lt where Trade.tid=lt.tid
alter table Trade alter column targetDate set not null

alter table Trade rename column expiration to targetDate
===#