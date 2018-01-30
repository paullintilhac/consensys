#include <string>
#include <fstream>
#include <iostream>	
#include <sstream>
#include <algorithm>
using namespace std;

class Reader{
public:
	string fileName;
	vector<vector<string>> data;
	vector<string> uniqueAddresses;
	Reader(string f1){
		this->fileName = f1;
		data = read(f1);
	}

	vector<vector<string>> read(string fileName){
		ifstream infile(fileName);
		
		string str;
		int count = 0;
		getline(infile, str);
		vector<vector<string>> data;
		
		while (getline(infile, str)) {
	    //cout<<++count<<endl;
		vector<string> thisLine;
		stringstream ss(str);

	   	string i;

	    while (getline(ss, i, ','))
	    {
	        thisLine.push_back(i);
	        if (ss.peek() == ',')
	            ss.ignore();
	    }

	    data.push_back(thisLine);
	    //cout<<"line size: "<<thisLine.size()<<endl;
	    if (find(uniqueAddresses.begin(), uniqueAddresses.end(), thisLine[5]) == uniqueAddresses.end())
		{
		  uniqueAddresses.push_back(thisLine[5]);
		}
		if (find(uniqueAddresses.begin(), uniqueAddresses.end(), thisLine[8]) == uniqueAddresses.end())
		{
		  uniqueAddresses.push_back(thisLine[8]);
		}
	    //cout<<"size "<<thisLine.size()<<endl;
		}
		cout<<uniqueAddresses.size()<<endl;
		return(data);
		//cout<<"size: "<<svmData.size()<<endl;
	}

};
