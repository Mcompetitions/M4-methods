#include <oxstd.oxh>
#import <database>

/** Load a file using loadmat
*/
Load(sFilename, sPrefix)
{
	decl asvar, data = loadmat(sFilename);

	if (data == 0)
		return {};

	println("\t", sizer(data), " rows in ", sizec(data), " columns in ", sFilename);
	asvar = new array[sizer(data)];
	for (decl i = 0; i < sizer(data); ++i)
		 asvar[i] = sprint(sPrefix, i + 1);
	
	return {data', asvar};
}

/** Save as a single file
*/
Save1(sFilename, mData, asNames, iFreq)
{
	// count valid observations, always valid from the beginning
	decl vt = sumc(!isdotnan(mData)), it1 = 0, i, it2;
	
	decl db = new Database();
	db.Create(iFreq, 1980, 1, 1990, 1);
		
	db.SetVar(mData, asNames);
	decl sfile = sFilename ~ ".in7";
	db.Save(sfile);
	println(sizec(mData), " variables with sample size <=", max(vt), " saved to ", sfile); 
		
	delete db;
}

/** Save in chunks
*/
Save(sFilename, mData, asNames, iFreq, cH=0, vTsubset=<>)
{
	// count valid observations, always valid from the beginning and without gaps
	decl vt = sumc(!isdotnan(mData)), ctot = sizer(mData), ct1 = 0, i, j, ct2;

	// create a copy that is aligned at the end
	decl mx = nans(mData);
	foreach (decl v in mData[][j])
		mx[ctot - vt[j] : ][j] = mData[ : vt[j] - 1][j];
	
	foreach (decl ct in vTsubset[i])
	{
		decl selidx = vecindex(vt .> ct1 .&& vt .<= ct);
		ct1 = ct;
		if (sizerc(selidx) == 0)
			continue;
		ct2 = max(vt[selidx]);

		selidx = sortbyc(vt[selidx]' ~ selidx, <0,1>)[][1];
		
		decl db = new Database();
		db.Create(iFreq, 1999, 1, 1999, iFreq);
		db.Grow(cH);					// add space at end for forecasts
		if (ct2 > iFreq)
			db.Grow(-(ct2 - iFreq));	// add space at start for observations
		
		db.SetVar(mx[ctot - ct2 : ][selidx], asNames[selidx]);
		decl sfile = sFilename ~ sprint("_", "%02d", i + 1) ~ ".in7";
		db.Save(sfile);
		println(sizerc(selidx), " variables with sample size <=", ct, " saved to ", sfile); 
		
		delete db;
	}
}
FixupDaily()
{
	println("Daily_06 --> Daily_07, splitting Daily_05");
	decl db7 = new Database();
	db7.Load("Daily_06.in7");
	db7.Save("Daily_07.in7");
	delete db7;

	decl db5 = new Database();
	db5.Load("Daily_05.in7");
	decl db6 = new Database();
	db6.Load("Daily_05.in7");

	decl c = idiv(db5.GetVarCount(), 2), as = db5.GetAllNames();

	db5.Remove(as[c : ]);
	db6.Remove(as[ : c - 1]);

	db5.Save("Daily_05.in7");
	db6.Save("Daily_06.in7");
	delete db5;
	delete db6;
}
main()
{
	decl asnames = {"Hourly","Daily","Monthly","Quarterly","Weekly","Yearly"};
	decl freqs = <24,5,12,4,52,1>, i;
	decl vh =    <48,14,18,8,13,6>;
	decl subsets = {<>, <1000,2500,3800,4196,4200>, <68,69,80,150,200,260,305,320,450,650>, <60,80,100,130>, <>, <18,25,30,40,60>};

	foreach (decl sname in asnames[i])
	{
		println("\nLoading:  ", sname);
		decl time = timer();
		decl [mdata, asvar] = Load("../data_original/" ~ sname ~ "-train.xlsx", sname[0:0]);

		println("in:", timespan(time), "\nSaving:   ", sname);
		time = timer();
		Save("../data/" ~ sname, mdata, asvar, freqs[i], vh[i], subsets[i] ~ 1e8);
		println("in:", timespan(time), "\nFinished: ", sname);
	}
	FixupDaily();
}
