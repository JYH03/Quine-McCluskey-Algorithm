// QMalgorithm.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <algorithm>
#include <string>
#include <fstream>
#include <vector>
using namespace std;

int combinedrow;
vector <vector<char>> combined_data;	//combined 데이터
void QMalgorithm(vector <vector<char>>_data, int row, int col)
{
	int i, j, k, l;
	int cnt = 0, nextrow = 0;
	int check;
	bool stop = false;
	int* ox = new int[row];
	vector <vector<char>> next_data;
	//QMalgorithm step 1~3 진행
	fill(ox, ox + (row - 1), 1);
	for (i = 0; i < row; i++)
	{
		stop = false;
		for (j = 0; j < row; j++)
		{
			cnt = 0;
			for (k = 2; k < col; k++)
			{
				//다른 부분 체크
				if (_data[i][k] != _data[j][k])		//서로 다르면 cnt 
				{
					check = k;
					cnt++;
				}
				if (cnt > 1)
				{
					break;
				}
			}
			//다른 부분이 하나면
			if (cnt == 1)
			{
				stop = true;			// 체크(v)
				ox[i] = 1;
				//다음 column의 row추가 및 col크기 할당
				next_data.push_back(vector<char>());
				next_data[nextrow].resize(col);			//오류
				//다음 column에다 복사 일치하는 부분은 -
				for (k = 0; k < col; k++)
				{
					if (k != check)
						next_data[nextrow][k] = _data[i][k];
					else
						next_data[nextrow][k] = '-';
				}
				//중복 검사
				for (k = 0; k < nextrow; k++)
				{
					for (l = 2; l < col; l++)
					{
						if (next_data[nextrow][l] != next_data[k][l])
						{
							break;
						}
					}
					if (l == col)
					{
						next_data.pop_back();
						nextrow--;
						break;
					}
				}
				nextrow++;
			}
		}
		//새 값을 생성 못하면 체크(*)하고 값 반환
		if (stop != true)
		{
			ox[i] = 0;
			combined_data.push_back(vector<char>(col));
			for (l = 2; l < col; l++)
				combined_data[combinedrow][l - 2] = _data[i][l];
			combinedrow++;
		}
	}
	//모두 0이면(모두 v가 아닌 *이면) QMalgorithm 종료
	for (i = 0; i < row; i++)
	{
		if (ox[i] == 1)
		{
			break;
		}
	}
	//메모리 해제
	_data.clear();
	_data.shrink_to_fit();
	delete[] ox;
	//더 이상 진행 불가능하면 반환 그렇지 않으면 재귀
	if (i == row)
	{
		next_data.clear();
		next_data.shrink_to_fit();
		return;
	}
	else QMalgorithm(next_data, nextrow, col);
}
int main()
{
	//변수
	int i = 0, j = 0, k = 0, l = 0, nextrow = 0, size = 0;
	int cnt = 0, bit, check = 0, row = 0, col, minrow = 0, key = 0;
	char* arr;
	vector <vector<char>> _data;			//처음 파일에서 입력받을 데이터
	vector <vector<char>> min_data;
	//스트림 생성
	ifstream fin;
	fin.open("input_minterm.txt");
	//데이터 입력받기, 동적 할당(벡터)
	fin >> bit;
	fin.ignore();
	col = bit + 3;
	arr = new char[col];
	while (!fin.eof())
	{
		fin.getline(arr, col);
		_data.push_back(vector <char>());
		for (i = 0; i < col; i++)
		{
			_data[row].push_back(arr[i]);
		}
		row++;
	}
	delete[]arr;
	fin.close();
	//QM algorithm 1~3
	QMalgorithm(_data, row, col);
	//QM algorithm 4
	//minterm 구하기
	for (i = 0; i < row; i++)
	{
		if (_data[i][0] == 'm')
		{
			min_data.push_back(vector<char>());
			min_data[minrow].resize(col);
			for (j = 2; j < col; j++)
			{
				min_data[minrow][j - 2] = _data[i][j];
			}
			minrow++;
		}
	}
	//minterm과 combined 비교
	vector <vector <int>> PItable(combinedrow, vector <int>(minrow, 0));
	for (i = 0; i < combinedrow; i++)
	{
		for (j = 0; j < minrow; j++)
		{
			for (k = 0; k < col - 2; k++)
			{
				//-면 다음 글자 검사
				if (combined_data[i][k] == '-')
				{
					continue;
				}
				//한 글자라도 다르면 다음 검사로 진행
				else if (combined_data[i][k] != min_data[j][k])
				{
					break;
				}
			}
			//검사가 끝까지 진행 = 전부 같음
			if (k == col - 2)
			{
				PItable[i][j] = 1;
			}
		}
	}
	//QM algorithm 5~7
	cnt = 0;
	ofstream fout;
	vector <int> PIs(combinedrow, 1);		//잡힌 PIs의 개수
	vector <int> mincheck(minrow, 1);	//잡힌 minterm의 개수
	vector <int> mincnt;		//minterm 아래 X 개수
	vector <vector<char>> transistor(combinedrow, vector <char>(col - 2));	//트랜지스터 계산용
	fout.open("result.txt");
	while (1)
	{
		cnt = 0;
		//minterm이 모두 잡히면
		for (i = 0; i < minrow; i++)
		{
			if (mincheck[i] == 0)
				cnt++;
		}
		//step 종료
		if (i == cnt)
		{
			break;
		}
		cnt = 0;
		//각 행에서
		for (i = 0; i < combinedrow; i++)
		{
			//이미 사용, 출력된 PIs면 continue
			if (PIs[i] == 0)
			{
				mincnt.push_back(0);
				continue;
			}
			//각 열에서
			for (j = 0; j < minrow; j++)
			{
				//잡지 않는 minterm 아래의 X
				if (PItable[i][j] == 1 && mincheck[j] == 1)
				{
					cnt++;	//개수 카운트
				}
			}
			//각 행의 miterm을 잡지않은 X 개수 저장
			mincnt.push_back(cnt);
			cnt = 0;
		}
		//가장 많은 miterm을 잡을 수 있는 X가 있는 행의 위치
		size = max_element(mincnt.begin(), mincnt.end()) - mincnt.begin();
		//사용한 PIs임을 표시
		PIs[size] = 0;
		//X면서 잡지 않은 minterm 아래면 
		for (i = 0; i < minrow; i++)
		{
			if (PItable[size][i] == 1 && mincheck[i] == 1)
			{
				//minterm 잡기
				mincheck[i] = 0;
			}
		}
		//가장 많은 miterm을 잡을 수 있는 X가 있는 PIs
		for (i = 0; i < col - 2; i++)
		{
			//트랜지스터 계산을 위해 저장
			transistor[l][i] = combined_data[size][i];
			//출력
			fout << combined_data[size][i];
		}
		fout << endl;
		l++;
		//mincnt 초기화
		mincnt.clear();
		mincnt.shrink_to_fit();
	}
	//트랜지스터 개수 
	size = 0;
	for (i = 0; i < l; i++)
	{
		transistor[i].pop_back();
		transistor.shrink_to_fit();
	}
	fout << "Cost (# of transistors): ";
	//OR
	if (l >= 2)
	{
		size = 2 * l + 2;
	}
	//AND
	if (l >= 2)
	{
		for (i = 0; i < l; i++)
		{
			cnt = 0;
			for (j = 0; j < col - 3; j++)
			{
				if (transistor[i][j] != '-')
					cnt++;
			}
			size += 2 * cnt + 2;
		}
	}
	//not
	for (i = 0; i < col - 3; i++)
	{
		cnt = 0;
		for (j = 0; j < l; j++)
		{
			if (transistor[j][i] == '0')
			{
				cnt = 1;
			}
		}
		size += 2 * cnt;
	}
	fout << size;
	fout.close();
}