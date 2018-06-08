#include <oxstd.oxh>
#include <oxdraw.oxh>
#include "M4.oxh"

/** Load all data files of the specified type into an array of databases.
*/
M4::M4(const sType, const fnNewDb)
{
	decl sfile = sm_sPath ~ sType, asdb = {};

	m_sType = sType;
	m_aoDb = {};

	Modelbase::ShowBanner(FALSE);
	
	if (sType != "")
	{
		for (decl i = 0;; ++i)
		{
			decl sfile_i = sprint(sfile, "_", "%02d", i + 1, ".in7");
			decl db = fnNewDb();
			if (!db.Load(sfile_i))
				break;
			m_aoDb ~= db;
			println("loaded ", sfile_i, " with ", db.GetVarCount(), " series");
		}
	}
	m_mResult = <>;
	m_asY = {};
	m_asR = {};
}
/** Sets the root path for the data files
*/
M4::SetDataPath(const sPath)
{
	sm_sPath = sPath;
}
/** Sets the root path for the data files
*/
M4::GetDataPath()
{
	return sm_sPath;
}
/** Load results from previously saved files.
*/
M4::LoadResults(const sType, const sSubType)
{
	decl db = new Database();
	
	m_sType = sType;
	if (db.Load(sType ~ sSubType ~ ".in7"))
	{
		println("Loaded results from: ", db.GetDbName());
		m_asR = db.GetAllNames()[1 : ];
		m_mResult = db.GetAll()[][1 : ]';
		m_asY = db.GetVarChoices("Names")[db.GetVar("Names")];

		if (ismissing(m_mResult))
			println("Warning: data has missing values");
	}
	delete db;
}

M4::~M4()
{
	foreach (decl db in m_aoDb)
		delete db;
}

/**  Aggregate a database to annual data.
@param db in: database, out: modified.
@param sMethod in: 0: end of period, 1: take average (default)
@param cTmin in: remove variables with fewer than cTmin observations
*/
M4::TransformDb(const db, const sMethod, const cTmin)
{
	decl mx = db.GetAll();
	decl mask = isdotnan(mx);
	decl mx0 = mask .? 0 .: mx;
	
	switch_single(sMethod)
	{
		case "DS" :	 mx = diff(mx, db.GetFrequency());
		case "D" :	 mx = diff(mx);
		case "DD" :	 mx = diff(diff(mx));
		case "DDS" : mx = diff(diff(mx, db.GetFrequency()));
		case "DSL" : mx = diff(log(mx), db.GetFrequency());
		case "DL" :	 mx = diff(log(mx));
		case "inv" : mx = 1 ./ mx;
		case "sqrt": mx = sqrt(mx);
		case "sqr":  mx = sqr(mx);
		case "log":  mx = log(mx);
		case "cum":	 mx0 = cumulate(mx0); mx = mask .? .NaN .: mx0;
		case "ran1": mx0 = diff0(mx0); mx0 = cumulate(mx0 + rann(sizer(mx), sizec(mx)) .* sqrt(varc(deleter(mx))));	mx = mask .? .NaN .: mx0;
		case "ran2": mx0 = diff0(mx0); mx0 = cumulate(mx0 + meanc(deleter(mx)) + rann(sizer(mx), sizec(mx)) .* sqrt(varc(deleter(mx)))); mx = mask .? .NaN .: mx0;
		case "rev":  mx = reversec(mx); 
	}
	db.RenewBlock(mx, 0);

	// only keep variables that have enough observations
	decl vdrop = vecindex(sumc(!isdotnan(mx)) .< cTmin);
	db.Remove(db.GetAllNames()[vdrop]);
}

/**  Aggregate a database to annual data.
@param db in: database, out: modified.
@param iAverage in: 0: end of period, 1: take average (default)
@param cTmin in: remove variables with fewer than cTmin observations
*/
M4::AnnualizeDb(const db, const iAverage, const cTmin)
{
	decl iper0 = db.ObsPeriod(0), iper1 = db.ObsPeriod(db.GetSize() - 1),
		freq = db.GetFrequency(), cx = db.GetVarCount();
	decl mx = nans(iper0 - 1, cx) | db.GetAll() | nans(freq - iper1, cx);		// whole years
	decl years = idiv(sizer(mx), freq);
	decl mxy = zeros(years, cx), i, vx;
	if (iAverage == 1)
		parallel foreach (vx in mx[][i])
			mxy[][i] = meanr(reshape(vx, years, freq));
	else
		parallel foreach (vx in mx[][i])
			mxy[][i] = reshape(vx, years, freq)[][freq - 1];

	db.Shrink(db.GetSize() - years - 6);
	db.Resample(1, db.ObsYear(0), 1);
	db.RenewBlock(mxy | nans(6, cx), 0);

	// only keep variables that have enough observations
	decl vdrop = vecindex(sumc(!isdotnan(mxy)) .< cTmin);
	db.Remove(db.GetAllNames()[vdrop]);
}

/**  Transform the databases
@param sTransform in: string with transformation, currently: "DS" "D" "DSL" "DL" "inv" "sqrt"
*/
M4::Transform(const sTransform)
{
	if (isstring(sTransform))
	{
		parallel foreach (decl db in m_aoDb)
		{
			TransformDb(db, sTransform, 3 * db.GetFrequency());
		}
	}
}

/**  Aggregate the databases to annual data.
@param iAverage in: 0: end of period, 1: take average (default), 2: seasonal difference
*/
M4::Aggregate(const iAverage)
{
	foreach (decl db in m_aoDb)
	{
		AnnualizeDb(db, iAverage, 10);
	}
	m_sType = "Yearly";
}

/**  Runs a function on each ForecastABC object.
@param fnRun in: fnRun(const obj) function to apply<br>
fnRun should return array[2] {matrix, array of strings}
@param asR array of row labels
@comment stores the results, use `M4::GetResults` and `M4::GetNames`
to get the results and names.
*/
M4::Apply(const fnRun, const asR)
{
	decl time = timer(), m, as;
	foreach (decl db in m_aoDb)
	{
		[m, as] = fnRun(db);
		m_mResult ~= m;
		m_asY ~= as;
	}
	m_asR ~= asR;

	println("Run done in ", timespan(time));
}
/**  Runs a function on each ForecastABC object, collecting the results.
@param fnRun in: fnRun(const obj) function to apply<br>
fnRun should return array[2] {matrix, array of strings}
*/
M4::CollectData(const fnRun, args)
{
	decl m = <>, as = {}, mi, asi;
	
	foreach (decl db in m_aoDb)
	{
		[mi, asi] = fnRun(db, args);
		// to avoid concatenation warning:
		if (sizer(mi) < sizer(m))
			mi |= nans(sizer(m) - sizer(mi), sizec(mi));
		else if (sizer(mi) > sizer(m))
			m |= nans(sizer(mi) - sizer(m), sizec(m));
		m ~= mi;
		as ~= asi;
	}
	return { m, as };
}
/**  Gets default forecast horizon.
*/
M4::GetH()
{
	switch_single(strlwr(m_sType))
	{
		case "hourly":		return 48;
		case "daily":		return 14;
		case "weekly":		return 13;
		case "monthly":		return 18;
		case "quarterly":	return  8;
		case "yearly":		return  6;
	}
}
/**  Gets maximum estimation sample size
*/
M4::GetLimit()
{
	decl years = 40;
	switch_single(strlwr(m_sType))
	{
		case "hourly":		return 5040;
		case "daily":		return 4 * 365;
		case "weekly":		return years * 52;
		case "monthly":		return years * 12;
		case "quarterly":	return years * 4;
		case "yearly":		return   years;
	}
}
/**  Gets maximum estimation sample size
*/
M4::GetFrequency()
{
	switch_single(strlwr(m_sType))
	{
		case "hourly":		return  24;
		case "daily":		return 365;
		case "weekly":		return  52;
		case "monthly":		return  12;
		case "quarterly":	return   4;
		case "yearly":		return   1;
	}
}
M4::GetResults() { return m_mResult; }
M4::GetNames()	 { return m_asY; }
M4::GetRowLabels()	 { return m_asR; }

/**  Creates an array of labels.
@param sLabel in: text with %s for replacement
@param aArgs in: array of strings with replacement text
*/
M4::Replicate(const sLabel, const aArgs)
{
	if (isarray(sLabel))
	{
		decl as = {};
		foreach (decl s in sLabel)
			as ~= Replicate(s, aArgs);
		return as;
	}

	decl as = new array[sizeof(aArgs)], i, s;
	foreach (s in aArgs[i])
		as[i] = replace(sLabel, "%s", s);
	return as;
}
/**  Gets the sort index of the list of M4 variables of a single frequency.
@param asY in: array[M] of strings with names
@returns matrix[1][M] with order
*/
M4::SortIndex(const asY)
{
	decl ct = sizeof(asY);

	decl var = zeros(1, ct), i, j, s;
	foreach (s in asY[i])
	{
		sscan(s[1:], "%d", &j);
		var[i] = j;
	}	
	return sortcindex(var);
}

/**  Saves the results to a file.
@param sType in: type of file (appended to base type)
@param asR in: array of strings with row labels
*/
M4::Save(const sType, const bSort)
{
	decl db = new Database(), ct = sizec(m_mResult);
	db.Create(ct);

	db.Append(range(0, ct - 1)', "Names");

	if (bSort)
	{
		decl var = zeros(1, ct), i, j, s;
		foreach (s in m_asY[i])
		{
			sscan(s[1:], "%d", &j);
			var[i] = j;
		}	
		decl idx = sortcindex(var);
		db.SetVarChoices("Names", m_asY[idx]);
		db.Append(m_mResult[][idx]', m_asR);
	}
	else
	{
		db.SetVarChoices("Names", m_asY);
		db.Append(m_mResult', m_asR);
	}
	db.SaveIn7(m_sType ~ sType);

	println("Results saved to ", m_sType ~ sType ~ ".in7");
}

/**  Runs a function on each database variable.
@param fnRun in: fnRun(const vY, const sY) function to apply<br>
fnRun should return a double
@comment missing values are removed from vY.
*/
M4::ApplyVar(fnRun)
{
	decl vall = <>;
	foreach (decl db in m_aoDb)
	{
		decl c = db.GetVarCount(), v = zeros(1, c), as = db.GetAllNames();
		for (decl i = 0; i < c; ++i)
			v[i] = fnRun(deleter(db.GetVarByIndex(i)), as[i]);
		vall ~= v;
	}
	return vall;
}

/**  Returns a variable from the database.
*/
M4::GetVar(const sVar)
{
	decl vall = <>;
	foreach (decl db in m_aoDb)
	{
		decl v = db.GetVar(sVar);
		if (sizerc(v) > 0)
			return v;
	}
	return <>;
}

M4::Plot(iPlot, vCrit, sTitle, cBlock, dMax)
{
	vCrit = sortr(vec(vCrit)');

	decl idx = isdotmissing(vCrit);
	if (sumr(idx))
	{
		println("*** Warning: ", sTitle, ": ", int(sumr(idx)), " missing values removed");
		vCrit = deleteifc(vCrit, idx);
	}
	if (dMax != .NaN)
	{
		decl idx = vCrit .> dMax;
		if (sumr(idx))
		{
			println("*** Warning: ", sTitle, ": ", int(sumr(idx)), " values in excess of ", dMax);
			vCrit = deleteifc(vCrit, idx);
		}
	}

	decl ct = sizerc(vCrit), chist = cBlock, vlo, vhi, sel1, sel2, cb, vx;

	if (cBlock <= 0)
	{
		cBlock = ceil(ct / 100);
		chist = 100;
	}
	sel1 = range(0, ct - 1, cBlock);
	sel2 = sel1[1 :] - 1 ~ ct - 1;
	vlo = vCrit[sel1];
	vhi = vCrit[sel2];
	cb = sizerc(sel1);
	vx = range(1, cb) / cb;

	decl lo = vCrit[0], hi = vCrit[ct - 1], dx = (hi - lo) / chist;
	decl vbar = discretize(vCrit, lo, hi, chist, 0) / ct;
	decl vbx = range(1, chist - 1) * dx + lo; 
	DrawXMatrix(iPlot, vec(vbx | vbx | vbx)', "", vec(zeros(vbar) | vbar | nans(vbar))', "");
	DrawAdjust(ADJ_COLOR, 14, 12);
	DrawAxisAuto(iPlot, 0);
	DrawAdjust(ADJ_AXISGRID, 1);

	DrawTitle(iPlot, sTitle);
	DrawXMatrix(iPlot, vhi + dx / 4, "", vx, "");
	DrawAdjust(ADJ_COLOR, 2, 13);
	DrawZ(vlo - dx / 4, "", ZMODE_HILO);

	DrawAdjust(ADJ_AREA_X, iPlot, -0.02, 1.02);

	return 1;
}
