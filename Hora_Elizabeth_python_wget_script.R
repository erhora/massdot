# Python Script Used to Download Data -------------------------------------
# Python Code:
import wget

years = list(range(2016,2021))
months = list(range(1,13))
location_list = ["AET09", "AET10", "AET11", "AET12", "AET13"]
direction_list = ["EB", "WB"]


for locationcode in location_list:
  for direction in direction_list:
  for year in years:
  for month in months:
  datecode = str(year)+"-"+str(month).zfill(2)+"-01"
url_base = "https://mhd.ms2soft.com/tcds/file_download.aspx?agency_id=96&tcds_user_type="
url_volume = url_base + "USER&format=excel&type=MonthlyVolume&abn=1&local_id=" + locationcode + "_" + direction + "&sdate=" + datecode
url_speed = url_base + "USER&format=excel&type=MonthlyBin&i=8213126&abn=1&local_id=" + locationcode + "_" + direction + "&sdate=" + datecode +"&count_type=speed"
url_class = url_base + "USER&format=excel&type=MonthlyBin&abn=1&local_id=" + locationcode + "_" + direction + "&sdate=" + datecode +"&count_type=class"
wget.download(url_volume, 'c:/data/MonthlyVolumeReport_'+locationcode+"_"+direction+"_"+datecode+'.xlsx')
wget.download(url_speed, 'c:/data/MonthlySpeedReport_'+locationcode+"_"+direction+"_"+datecode+'.xlsx')
wget.download(url_class, 'c:/data/MonthlyClassReport_'+locationcode+"_"+direction+"_"+datecode+'.xlsx')

