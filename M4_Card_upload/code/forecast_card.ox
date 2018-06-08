#include <oxstd.oxh>
#import "ForecastAB"
#import "M4"

ForecastFrequency(const sType, const sMethod, const sForc)
{
	decl m4 = new M4(sType, [=]() { return new ForecastAB();});

	decl isel;
	if (sForc == "LO")
		isel = 1;
	else if (sForc == "HI")
		isel = 2;
	else if (sForc == "")
		isel = 0;
	else
		oxrunerror("sForc argument");
		

	decl fn_run = [=](const oDB)
	{
		decl asy = oDB.GetAllNames(), ch = m4.GetH();

		if (sizeof(asy) == 0)
			continue;
		
		oDB.Select("Y", asy);

		oDB.AddForecasts(ch, {sMethod}, oDB.Forecast);
		return {constant(ch, 1, sizeof(asy)), asy};
	};
	decl fn_get_forc = [=](const oDB, const sType)
	{
		return {oDB.CollectForecasts(sType)[isel], oDB.GetVarNames() };
	};

	m4.Apply(fn_run, {sMethod});
	decl mforc, asforc, idx;
	[mforc, asforc] = m4.CollectData(fn_get_forc, sMethod);
	idx = m4.SortIndex(asforc);
	
	delete m4;

	return {mforc[][idx]', asforc[idx]};
}

SaveForecasts(const asTypes, const sMethod, const sForc="", sFilename="")
{
	decl mall = <>, asall = {}, i, v, s, time = timer();

	// already open file to check before forecasting
	if (sFilename == "")
	{
		if (find(M4::GetDataPath(), "M3") >= 0)
			sFilename += "M3_";
		else
			sFilename += "M4_";
		sFilename += "Doornik_";
		sFilename += sMethod;
		if (sForc != "")
			sFilename += sForc;
		sFilename += ".csv";
	}
	
	decl f = fopen(sFilename, "wV");

	// collect all the forecast in the correct order
	foreach (decl stype in asTypes)
	{
		decl mforc, asforc;
		[mforc, asforc] = ForecastFrequency(stype, sMethod, sForc);
		if (sizec(mforc) < sizec(mall))
			mforc ~= nans(sizer(mforc), sizec(mall) - sizec(mforc));
		else
			mall ~= nans(sizer(mall), sizec(mforc) - sizec(mall));
		mall |= mforc;
		asall ~= asforc;
	}
	// now save them as a csv
	fprint(f, "id");
	for (i = 0; i < sizec(mall); ++i)
		fprint(f, ",F", i + 1);
	fprint(f, "\n");

	foreach (v in mall[i][])
	{
		s = sprint(asall[i], ",", "%cs", ",", "%rs", "", "%.10g", v, "\n");
		s = replace(s, ".NaN", "NA");
		fprint(f, s);
	}
	fclose(f);
	println(sizer(mall), " ", sMethod, " forecasts stored to ", sFilename, " in ", timespan(time));
}
main()
{
	decl astypes = {"Yearly","Quarterly","Monthly","Weekly","Daily","Hourly"};

//	M4::SetDataPath("../data_M3/");

	// this means we produce the same forecasts three times.
	// However, we only need to do this once, so no need to optimize.
	SaveForecasts(astypes, "Card");
	SaveForecasts(astypes, "Card", "LO");
	SaveForecasts(astypes, "Card", "HI");
}
